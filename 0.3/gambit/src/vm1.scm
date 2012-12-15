;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          vm.scm
;;;; Project:       Bard
;;;; Purpose:       bard virtual machine
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

;;; ---------------------------------------------------------------------
;;; prelude
;;; ---------------------------------------------------------------------

(define-macro (^ params . body)
  `(lambda ,params ,@body))

(define-macro (unless test . body)
  `(if (not ,test)
       (begin
         ,@body)))

(define-macro (when test . body)
  `(if ,test
       (begin
         ,@body)))

(define-macro (prog1 form . forms)
  `(let ((val ,form))
     (begin ,@forms)
     val))

;;; ---------------------------------------------------------------------
;;; vmstate
;;; ---------------------------------------------------------------------

(define-type vmstate
  constructor: %private-make-vmstate
  (code %code %setcode!)
  (pc %pc %setpc!)
  (instr %instr %setinstr!)
  (fn %fn %setfn!)
  (nargs %nargs %setnargs!)
  (env %env %setenv!)
  (globals %globals %setglobals!)
  (stack %stack %setstack!)
  (cmd %cmd %setcmd!))

(define (%makevmstate fn env globals exitfn)
  (let* ((code (if fn (%fn-code fn) #f))
         (nargs (if fn (%fn-nargs fn) 0))
         (pc 0)
         (instr #f)
         (stack '()))
    (%private-make-vmstate code pc instr fn nargs env globals stack exitfn)))

(define-type savedstate
  constructor: %private-make-saved
  (code %saved-code %set-saved-code!)
  (pc %saved-pc %set-saved-pc!)
  (fn %saved-fn %set-saved-fn!)
  (env %saved-env %set-saved-env!))

(define (%make-saved vmstate)
  (vector (%fn vmstate)(%code vmstate)(%env vmstate)(%pc vmstate)))

(define (%restore-saved! vmstate saved)
  (%setfn! vmstate (%saved-fn saved))
  (%setcode! vmstate (%saved-code saved))
  (%setenv! vmstate (%saved-env saved))
  (%setpc! vmstate (%saved-pc saved)))

;;; accessors
;;; ---------------------------------------------------------------------

(define (%incvm! state)(%setpc! state (+ 1 (%pc vm))))
(define (%push! state v)(%setstack! state (cons v (%stack state))))
(define (%top state)(car (%stack state)))
(define (%pop! state)(prog1 (%top state)(%setstack! state (cdr (%stack state)))))
(define (%swap! state)(%setstack! state (cons (cadr (%stack state))(cons (car (%stack state))(cddr (%stack state))))))
(define (%popenv! state)(prog1 (car (%env state))(%setenv! state (cdr (%env state)))))

;;; ---------------------------------------------------------------------
;;; vmops
;;; ---------------------------------------------------------------------

(define $opname->opfn-table (make-table test: eq?))
(define $opfn->opname-table (make-table test: eq?))

(define-macro (defop opname args . body)
  `(let ((opfn (lambda ,args ,@body)))
     (table-set! $opname->opfn-table ',opname opfn)
     (table-set! $opfn->opname-table opfn ',opname)
     ',opname))

(define (%opname->opfn opname)(table-ref $opname->opfn-table opname))
(define (%opfn->opname opfn)(table-ref $opfn->opname-table opfn 'HALT))

;;; ---------------------------------------------------------------------

(defop NOP    (s)   s)
(defop HALT   (s)   ((%cmd s) 'exit))
(defop CONST  (s k) (%push! s k))
(defop CLASS  (s c) (%push! s (%gset! (%globals s) c (%make-class c))))
(defop LREF   (s l) (%push! s (%lref (%env s) l)))
(defop LSET   (s l) (%push! s (%lset! (%env s) l (%pop! s))))
(defop GREF   (s g) (%push! s (%gref (%globals s) g)))
(defop GSET   (s g) (%push! s (%gset! (%globals s) g (%pop! s))))
(defop POP    (s)   (%pop! s))
(defop PRIM   (s p) (%push! state (%apply-prim state p)))
(defop JUMP   (s d) (%setpc! state d))
(defop FJUMP  (s d) (unless (%pop! s)(%setpc! s d)))
(defop TJUMP  (s d) (when (%pop! s)(%setpc! s d)))
(defop SAVE   (s d) (let ((saved (%make-saved s)))
                      (%set-saved-pc! saved d)
                      (%push! s saved)))
(defop CALL   (s n) (%popenv! s)
                    (let ((f (%pop! s)))
                      (%setfn! s f)
                      (%setcode! s (%fn-code f))
                      (%setenv! s (%fn-env f))
                      (%setpc! s 0)
                      (%setnargs! s n)))
(defop RETURN (s)   (%swap! s)(%restore-saved! s (%pop! s)))
(defop CC     (s)   (%push! s (let ((stack (%stack state))
                                    (saved (%make-saved state)))
                                (%makeprim name: "An anonymous continuation"
                                           required-arguments: #f
                                           rest-arguments: #t
                                           opname: 'cc
                                           side-effects: #t
                                           opfn: (lambda args
                                                   (%setstack! s stack)
                                                   (for-each (lambda (arg)(%push! s arg))
                                                             args)
                                                   (%restore-saved! s saved))))))


;;; ---------------------------------------------------------------------
;;; the vm
;;; ---------------------------------------------------------------------

(define (%instruction->string instr)
  (if (pair? instr)
      (let* ((opfn (car instr))
             (args (cdr instr))
             (opnm (%opfn->opname opfn)))
        (object->string `(,opnm ,@args)))
      (object->string instr)))

(define (%code->string code pc)
  (if code
      (let* ((code-strings (map %instruction->string (vector->list code)))
             (code-lines (map (lambda (c)(list "  " c (string #\newline))) code-strings)))
        (if pc (set-cdr! (list-ref code-lines pc) "->"))
        (str "(" (apply str (map (lambda (cl)(apply str cl)) code-lines)) ")"))
      "()"))

(define (%fn->string fn)
  (if fn
      (str "#<fn "
           (or (%fn-name fn) "An anonymous function")
           ">")
      ""))

(define (%globals->string globals)
  (with-output-to-string 
    '()
    (lambda ()
      (if (> (table-length globals) 0)
          (begin
            (table-for-each (lambda (k v)
                              (newline)
                              (display "  ")
                              (display k)
                              (display ": ")
                              (display (object->string v)))
                            globals))))))

(define (%env->string env)
  (with-output-to-string 
    '()
    (lambda ()
      (if (> (length env) 0)
          (begin
            (for-each (lambda (entry)
                        (let ((k (car entry))
                              (v (cdr entry)))
                          (newline)
                          (display "  ")
                          (display k)
                          (display ": ")
                          (display (object->string v))))
                      globals))))))

(define (%stack->string s)
  (object->string s))


(define (%show-vmstate state)
  (newline)
  (display "Bard VM v. 0.3")(newline)
  (display (str " instr: " (%instruction->string (%instr state))))(newline)
  (display (str " pc: " (%pc state)))(newline)
  (display (str " code: " (%code->string (%code state)(%pc state))))(newline)
  (display (str " fn: " (%fn->string (%fn state))))(newline)
  (display (str " nargs: " (object->string (%nargs state))))(newline)
  (display (str " env: " (%env->string (%env state))))(newline)
  (display (str " globals: " (%globals->string (%globals state))))(newline)
  (display (str " stack: " (%stack->string (%stack state))))(newline)
  (newline))

(define (%decode instruction)
  (values (car instruction)
          (cdr instruction)))

(define (%linkfn vmstate fn)
  (%makefn parameters: (%fn-parameters fn)
           code: (vector-for-each (lambda (instr)
                                    (if (eq? 'HALT (car instr))
                                        (cons (%exit vmstate)
                                              (cdr instr))
                                        (cons (%opname->opfn (car instr))
                                              (cdr instr))))
                                  (%fn-code fn))
           name: (%fn-name fn)
           env: (%fn-env fn)))

(define (%promptread)
  (newline)
  (display "bardvm> ")
  (read-line))

(define (%vmload str)
  (let ((expr (call-with-input-string str (lambda (s)(read s)))))
    (cond
     ((string? expr) )
     ((pair? expr) )
     (else (error (str "Invalid load input: " str))))))

(define (%handlecmd cmd-line vmstate vmops)
  (let ((cmdlist (words (ltrim cmd-line))))
    (if (not (null? cmdlist))
        (let ((cmd (car cmdlist))
              (args (cdr cmdlist)))
          (cond
           ((string=? "?" cmd)(%print-vm-help))
           ((string=? "help" cmd)(%print-vm-help))
           ((string=? "l" cmd)(%vmload vmstate vmops (car args)))
           ((string=? "q" cmd)((%cmd vmstate) 'quit))
           ((string=? "quit" cmd)((%cmd vmstate) 'quit))
           ((string=? "r" cmd) #f)
           ((string=? "s" cmd) #f)
           ((string=? "show" cmd)(%show-vmstate vmstate))
           ((string=? "v" cmd)(newline)(display (str "Bard VM v" $bard-version-string)))
           (else (newline)
                 (display (str "ERROR: unrecognized command: " trimmed-cmd))))))))

(define (%print-vm-help)
  (newline)
  (display "  version - print the VM version")(newline)
  (display "  load PATH - load the Bard object-code file at PATH")(newline)
  (display "  run - start execution of the currently-loaded program")(newline)
  (display "  step - execute a single instruction and then print the VM state")(newline)
  (display "  show - print the current state of the VM")(newline)
  (display "  quit - exit the VM")(newline)
  (display "  help - print this help message")(newline))

(define (%makevm #!key (fn #f)(env (%null-env))(globals (%bard-globals)))
  (lambda ()
    (call/cc 
     (lambda (exit)
       (let* ((state (%makevmstate fn env globals exit))
              (_step! #f))

         (letrec ((_link       (lambda (f)(%linkfn state f)))
                  (_fetch!     (lambda ()(%setinstr! state (%code-ref (%fn-code (%fn state))(%pc state)))))
                  (_inc!       (lambda ()(%setpc! state (+ 1 (%pc state)))))
                  (_exec!      (lambda ()(receive (op args)(%decode (%instr state))
                                                  (apply op args))))
                  (_show       (lambda ()(%show-vmstate state)))
                  (_stepfn     (lambda ()(_fetch!)(_inc!)(_exec!)))
                  (_stepshowfn (lambda ()(_fetch!)(_inc!)(_exec!)(_show)))
                  (_run!       (lambda ()(let loop ()(_step!)(loop))))
                  ($_ops (vector _link _fetch! _inc! _exec! _show _step! _run!))
                  (_shell      (lambda ()(let loop ()(%handlecmd (%promptread) state $_ops)(loop))))
                  (_cmd        (lambda msg
                                 (let ((cmd (car msg))
                                       (args (cdr msg)))
                                   (case cmd
                                     ((quit)(exit))
                                     (else (newline)(display (str "ERROR: unrecognized VM command: " cmd))))))))

           (%setcmd! state _cmd)
           (_shell)))))))

(define (%runvm #!optional (vm (%makevm)))(vm))
