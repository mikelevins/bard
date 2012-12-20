;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          instr.scm
;;;; Project:       Bard
;;;; Purpose:       bard virtual machine instructions
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
;;; vm instructions
;;; ---------------------------------------------------------------------

(define $opname->opfn-table (make-table test: eq?))
(define $opfn->opname-table (make-table test: eq?))

(define-macro (defop opname arglist . body)
  (let ((opfn (gensym)))
    `(begin
       (define ,opname #f)
       (let ((,opfn (lambda ,arglist (begin ,@body))))
         (set! ,opname ,opfn)
         (table-set! $opname->opfn-table ',opname ,opfn)
         (table-set! $opfn->opname-table ,opfn ',opname)
         ',opname))))

(defop NOP    (vm)  vm)
(defop HALT   (vm)  ((%exitfn vm)))
(defop CONST  (vm k) (%pushv! k vm))
(defop PRIM   (vm p) (%pushv! (%apply-prim p (%popnv! (%nargs vm) vm)) vm))
(defop LREF   (vm l) (%pushv! (%lref (%env vm) l) vm))
(defop LSET   (vm l) (%pushv! (%lset! (%env vm) l (%popv! vm)) vm))
(defop GREF   (vm g) (%pushv! (%gref (%globals vm) g) vm))
(defop GSET   (vm g) (%pushv! (%gset! (%globals vm) g (%popv! vm)) vm))
(defop GO     (vm d) (%setpc! vm d))
(defop FGO    (vm d) (unless (%popv! vm)(%setpc! vm d)))
(defop TGO    (vm d) (when (%popv! vm)(%setpc! vm d)))
(defop SAVE   (vm d) (%pushc! (%make-saved vm d)))
(defop CALL   (vm n) (let* ((f (%popv! vm)))
                       (%setfn! vm f)
                       (%setenv! vm (%make-call-env vm (%fn-env f) found-arg-count))
                       (%setpc! vm 0)))
(defop RETURN (vm)   (let ((saved (%popc! vm)))
                       (%setfn! vm (%fn saved))
                       (%setenv! vm (%env saved))
                       (%setpc! vm (%pc saved))))
(defop CC     (vm d) (%pushv! (%makecc vm d) vm))
(defop SETCC  (vm)   (let ((k (%popc! vm)))
                       (%setfn! vm (%fn (%cc-vmstate k)))
                       (%setenv! vm (%env (%cc-vmstate k)))
                       (%setstack! vm (%cc-stack k))
                       (%setpc! vm (%pc (%cc-vmstate k)))))

(define (%decode instruction)
  (values (car instruction)
          (cdr instruction)))

(define (%opname->opfn opname)(table-ref $opname->opfn-table opname #f))
(define (%opfn->opname opfn)(table-ref $opfn->opname-table opfn #f))

(define (%op? thing)(and (%opname->opfn thing) #t))

(define (%instr->string instr)
  (if instr
      (receive (op args)(%decode instr)
               (cond
                ((procedure? op)(%instr->string (cons (%opfn->opname op) args)))
                ((symbol? op)(object->string instr))
                (else (error (str "Unrecognized instruction: " instr)))))
      ""))
