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
;;; macros
;;; ---------------------------------------------------------------------

(define-macro (unless test . body)
  `(if (not ,test)
       (begin
         ,@body)))

(define-macro (when test . body)
  `(if ,test
       (begin
         ,@body)))

;;; ---------------------------------------------------------------------
;;; vmstate
;;; ---------------------------------------------------------------------

(define-type %vmstate
  constructor: %private-make-vmstate
  (%instr %instr %setinstr!)
  (%code %code %setcode!)
  (%pc %pc %setpc!)
  (%fn %fn %setfn!)
  (%env %env %setenv!)
  (%globals %globals %setglobals!)
  (%vstack %vstack %setvstack!)
  (%cstack %cstack %setcstack!)
  (%exitfn %exitfn %setexitfn!))

(define (%makevmstate fn env globals)
  (let* ((code (%fn-code fn))
         (pc 0)
         (instr (vector-ref code pc))
         (vstack '())
         (cstack '())
         (exitfn #f))
    (%private-make-vmstate instr code pc fn env globals vstack cstack exitfn)))

(define (%make-saved state)
  (%private-make-vmstate (%instr state) (%code state) (%pc state) (%fn state) (%env state)
                         (%globals state) (%vstack state) (%cstack state) (%exitfn state)))

(define (%copystate! srcstate deststate)
  (%setinstr! deststate (%instr srcstate))
  (%setcode! deststate (%code srcstate))
  (%setpc! deststate (%pc srcstate))
  (%setfn! deststate (%fn srcstate))
  (%setenv! deststate (%env srcstate))
  (%setglobals! deststate (%globals srcstate))
  (%setvstack! deststate (%vstack srcstate))
  (%setcstack! deststate (%cstack srcstate))
  (%setexitfn! deststate (%exitfn srcstate)))

(define (%op state)(car (%instr state)))
(define (%arg1 state)(list-ref (%instr state) 1))
(define (%arg2 state)(list-ref (%instr state) 2))
(define (%code-ref state i)(vector-ref (%code state) i))
(define (%pushv! state val)(%setvstack! state (cons val (%vstack state))))
(define (%topv state)(car (%vstack state)))
(define (%popv! state)(let ((val (car (%vstack state)))) (%setvstack! state (cdr (%vstack state))) val))
(define (%takeallv! state)(let ((vals (%vstack state))) (%setvstack! state '()) vals))
(define (%pushc! state val)(%setcstack! state (cons val (%cstack state))))
(define (%topc! state)(car (%cstack state)))
(define (%popc! state)(let ((saved (car (%cstack state)))) (%setcstack! state (cdr (%cstack state))) saved))
(define (%cc state)(%pushv! state (%make-saved state)))
(define (%setcc! state)(%copystate! (%popv! state) state))

(define (%apply-prim state)
  (let* ((pname (%arg1 state))
         (prim (%getprim pname))
         (pfn (%prim-opfn prim))
         (args (%takeallv! state)))
    (apply pfn args)))

;;; ---------------------------------------------------------------------
;;; vm ops
;;; ---------------------------------------------------------------------

(define $opname->opfn-table (make-table test: eq?))
(define $opfn->opname-table (make-table test: eq?))

(define-macro (defop opname args . body)
  `(let ((opfn (lambda (vmstate ,@args) (let ((%state (lambda () vmstate))) ,@body))))
     (table-set! $opname->opfn-table ',opname opfn)
     (table-set! $opfn->opname-table (table-ref $opname->opfn-table ',opname) ',opname)
     ',opname))

(define (%opname->op opname)
  (table-ref $opname->opfn-table opname))

(define (%opfn->opname opfn)
  (table-ref $opfn->opname-table opfn 'HALT))

(defop NOP    ()     (%state))
(defop HALT   ()     ((%exitfn (%state))(%state)))
(defop CONST  (k)    (%pushv! (%state) k))
(defop CLASS  (nm)   (%pushv! (%state)(%defglobal! (%globals (%state)) nm (%make-class nm) mutable: #t)))
(defop LREF   (i j)  (%pushv! (%state) (%lref (%env (%state)) i j)))
(defop LSET   (i j)  (%lset! (%env state) i j (%topv (%state))))
(defop LSETR  (i j)  (%pushv! (%state) (%lsetter (%env state) i j)))
(defop GREF   (g)    (%pushv! (%state) (%gref (%env state) g)))
(defop DEF    (g m?) (%pushv! (%state) (%gdef! (%globals state) g (%popv! state) mutable: m?)))
(defop GSET   (g)    (%pushv! (%state) (%gset! (%globals state) g (%topv state))))
(defop GSETR  (g)    (%pushv! (%state) (%gsetter (%globals state) g)))
(defop POPV   ()     (%popv! (%state)))
(defop POPC   ()     (%popc! (%state)))
(defop PRIM   ()     (%pushv! (%state) (%apply-prim (%state))))
(defop JUMP   (d)    (%setpc! (%state) d))
(defop FJUMP  (d)    (unless (%popv! (%state))(%setpc! (%state) d)))
(defop TJUMP  (d)    (when (%popv! state)(%setpc! (%state) d)))
(defop CALL   () (error "CALL not yet implemented"))
(defop RETURN () (error "RETURN not yet implemented"))
(defop SPAWN  () (error "SPAWN not yet implemented")) ; create an actor
(defop SEND   () (error "SEND not yet implemented")) ; send a value to an actor (enqueue it on the target's mailbox)
(defop RECV   () (error "RECV not yet implemented")) ; pop a value from the VM's mailbox
(defop READ   () (%pushv! (%state) (bard:read (%popv! (%state)))))
(defop WRITE  () (error "WRITE not yet implemented")) ; write a Bard object to a port

;;; ---------------------------------------------------------------------
;;; vm execution
;;; ---------------------------------------------------------------------

(define (%decode instruction)
  (values (car instruction)
          (cdr instruction)))

(define (%fetch vmstate n)
  (let ((instr (%code-ref (%code vmstate) n)))
    (%inc! vmstate)
    instr))

(define (%exec! instruction vmstate)
  (receive (op args)(%decode instruction)
           (op args vmstate)))

(define (%step! vmstate)
  (%exec! (%fetch! vmstate (%pc vmstate)) vmstate))

;;; ---------------------------------------------------------------------
;;; vm debug displays
;;; ---------------------------------------------------------------------

(define (%instruction->string instr)
  (if (pair? instr)
      (let* ((opfn (car instr))
             (args (cdr instr))
             (opnm (%opfn->opname opfn)))
        (object->string `(,opnm ,@args)))
      (object->string instr)))

(define (%code->string code)
  (str "("
       (apply str
              (interpose " " 
                         (map %instruction->string (vector->list code))))
       ")"))

(define (%fn->string fn)
  (str "#<fn "
       (or (%fn-name fn) "An anonymous function")
       ">"))

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

(define (%printstate state)
  (newline)
  (display "Bard VM v 0.3.0")(newline)
  (display (str " instr: " (%instruction->string (%instr state))))(newline)
  (display (str " code: " (%code->string (%code state))))(newline)
  (display (str " pc: " (%pc state)))(newline)
  (display (str " fn: " (%fn->string (%fn state))))(newline)
  (display (str " env: " (%env state)))(newline)
  (display (str " globals: " (%globals->string (%globals state))))(newline)
  (display (str " vstack: " (%vstack state)))(newline)
  (display (str " cstack: " (%cstack state)))(newline)
  (display (str " exitfn: " (%exitfn state)))(newline))
