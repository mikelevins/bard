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
;;; vm data structure
;;; ---------------------------------------------------------------------

(define-type vmstate
  constructor: %vmstate
  extender: defstate
  (fn %fn %setfn!)
  (pc %pc %setpc!)
  (env %env %setenv!))

(defstate vm
  constructor: %private-make-vm
  (instr %instr %setinstr!)
  (vals %vals %setvals!)
  (stack %stack %setstack!)
  (globals %globals %setglobals!)
  (initfn %initfn %setinitfn!)
  (exitfn %exitfn %setexitfn!)
  (stepfn %stepfn %setstepfn!)
  (runfn %runfn %setrunfn!)
  (prompt %prompt %setprompt!)
  (shellfn %shellfn %setshellfn!))

;;; ---------------------------------------------------------------------
;;; vm operations
;;; ---------------------------------------------------------------------

(define (%push! v vm)(%setstack! vm (cons v (%stack vm))))
(define (%top vm)(car (%stack vm)))
(define (%pop! vm)(prog1 (%top vm)(%setstack! vm (cdr (%stack vm)))))

(define (%pushv! v vm)(%setvals! vm (cons v (%vals vm))))
(define (%topv vm)(car (%vals vm)))
(define (%popv! vm)(prog1 (%topv vm)(%setvals! vm (cdr (%vals vm)))))

(define (%make-saved vm pc)(%vmstate (%fn vm) pc (%env vm)))

(define (%showvm vm)
  (newline)
  (display (str "Bard VM v" $bard-version-string))(newline)
  (display (str "pc: " (object->string (%pc vm))))(newline)
  (display (str "instr: " (%instr->string (%instr vm))))(newline)
  (display (str "vals: " (object->string (%vals vm)))))

(define (%showenv vm)
  (newline)
  (let ((env (%env vm)))
    (display (str "env:"))
    (if env
        (for-each (lambda (entry)
                    (let ((k (car entry))
                          (v (cdr entry)))
                      (newline)
                      (display (str "  " k ": " v))))
                  env))
    (newline)))

(define (%showglobals vm)
  (newline)
  (let ((globals (%globals vm)))
    (display (str "globals:"))
    (if globals
        (table-for-each (lambda (k v)
                          (newline)
                            (display (str "  " k ": " v)))
                        globals))
    (newline)))

(define (%showcode vm)
  (newline)
  (let* ((pc (%pc vm))
         (fn (%fn vm))
         (code (if fn (%method-code fn) #f)))
    (display (str "code:"))
    (if code
        (let ((codelen (vector-length code)))
          (let loop ((i 0))
            (if (< i codelen)
                (begin
                  (newline)
                  (if (= i pc)(display "->")(display "  "))
                  (display (%instr->string (vector-ref code i)))
                  (loop (+ 1 i)))))))
    (newline)))

(define (%promptread vm)
  (newline)
  (display (%prompt vm))
  (read-line))

(define (%ensure-fn! vm)
  (if (%fn vm)
      #t
      (let ((newfn (%make-method '() (vector))))
        (%setfn! vm newfn)
        #t)))

(define (%fn-append-instruction! vm new-instr)
  (let* ((old-code (%method-code (%fn vm)))
         (new-code (list->vector (append (vector->list old-code) (list (%link-instruction! new-instr))))))
    (%set-method-code! (%fn vm) new-code)
    (%fn vm)))

(define (%insert-instruction vm cmd-args)
  (if (null? cmd-args)
      (begin
        (newline)
        (display (str "ERROR: missing argument to insert")))
      (let* ((rd (lambda (s)(call-with-input-string s read)))
             (op (rd (car cmd-args)))
             (args (map rd (cdr cmd-args))))
        (%ensure-fn! vm)
        (%fn-append-instruction! vm (cons op args))
        (%fn vm))))

(define (%handlevmcmd vm cmdstr)
  (let ((cmd-line (words (ltrim cmdstr))))
    (if (null? cmd-line)
        (values)
        (let ((cmd (car cmd-line))
              (args (cdr cmd-line)))
          (cond
           ((string=? "insert" cmd)(%insert-instruction vm args))
           ((string=? "in" cmd)(%insert-instruction vm args))
           ((string=? "quit" cmd)(%exitvm vm))
           ((string=? "q" cmd)(%exitvm vm))
           ((string=? "show" cmd)(%showvm vm))
           ((string=? "sh" cmd)(%showvm vm))
           ((string=? "showcode" cmd)(%showcode vm))
           ((string=? "cd" cmd)(%showcode vm))
           ((string=? "showenv" cmd)(%showenv vm))
           ((string=? "en" cmd)(%showenv vm))
           ((string=? "showglobals" cmd)(%showglobals vm))
           ((string=? "gl" cmd)(%showglobals vm))
           ((string=? "step" cmd)(%step! vm))
           ((string=? "st" cmd)(%step! vm))
           (else (begin
                   (newline)
                   (display (str "Unrecognized VM command: " cmd)))))))))

;;; ---------------------------------------------------------------------
;;; vm constructor
;;; ---------------------------------------------------------------------

(define (%makevm #!key (fn #f)(env (%null-env))(globals (%bard-globals)))
  (let* ((vm (%private-make-vm fn 0 env #f '() '() globals #f #f #f #f "bard> " #f)))
    (%setinitfn! vm (lambda ()
                      (call/cc
                       (lambda (exit)
                         (letrec ((_fetch! (lambda ()(%setinstr! vm (%code-ref (%method-code (%fn vm))(%pc vm)))))
                                  (_inc! (lambda ()(%setpc! vm (+ 1 (%pc vm)))))
                                  (_exec! (lambda ()(receive (op args)(%decode (%instr vm))
                                                             (apply op vm args)))))
                           (%setexitfn! vm exit)
                           (%setstepfn! vm (lambda ()(_fetch!)(_inc!)(_exec!)))
                           (%setrunfn! vm (lambda ()(let loop ()(_fetch!)(_inc!)(_exec!)(loop))))
                           (%setshellfn! vm (lambda ()(let loop ()(%handlevmcmd vm (%promptread vm))(loop)))))))))
    vm))

;;; ---------------------------------------------------------------------
;;; vm control
;;; ---------------------------------------------------------------------

(define (%initvm! vm)
  ((%initfn vm))
  vm)

(define (%exitvm vm)
  ((%exitfn vm))
  vm)

(define (%step! vm)
  ((%stepfn vm))
  vm)

(define (%startvm vm)
  ((%shellfn vm))
  vm)


;;; (loadvm)
;;; (define $vm (%makevm))
;;; (%initvm! $vm)
;;; (%startvm $vm)



