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

(define NOP    (^ (vm )   vm))
(define HALT   (^ (vm )   ((%exitfn vm))))
(define CONST  (^ (vm k)  (%pushv! k vm)))
(define PRIM   (^ (vm p)  (%pushv! (%apply-prim p (%popnv! (%nargs vm) vm)) vm)))
(define LREF   (^ (vm l)  (%pushv! (%lref (%env vm) l) vm)))
(define LSET   (^ (vm l)  (%pushv! (%lset! (%env vm) l (%popv! vm)) vm)))
(define GREF   (^ (vm g)  (%pushv! (%gref (%globals vm) g) vm)))
(define GSET   (^ (vm g)  (%pushv! (%gset! (%globals vm) g (%popv! vm)) vm)))
(define GO     (^ (vm d)  (%setpc! vm d)))
(define FGO    (^ (vm d)  (unless (%popv! vm)(%setpc! vm d))))
(define TGO    (^ (vm d)  (when (%popv! vm)(%setpc! vm d))))
(define SAVE   (^ (vm d)  (%pushc! (%make-saved vm d))))

(define CALL   (^ (vm found-arg-count) 
                  (let* ((f (%popv! vm)))
                    (%setfn! vm f)
                    (%setenv! vm (%make-call-env vm (%fn-env f) found-arg-count))
                    (%setpc! vm 0))))

(define RETURN    (^ (vm)
                     (let ((saved (%popc! vm)))
                       (%setfn! vm (%fn saved))
                       (%setenv! vm (%env saved))
                       (%setpc! vm (%pc saved)))))

(define SETCC    (^ (vm)
                    (let ((k (%popc! vm)))
                      (%setfn! vm (%fn (%cc-vmstate k)))
                      (%setenv! vm (%env (%cc-vmstate k)))
                      (%setstack! vm (%cc-stack k))
                      (%setpc! vm (%pc (%cc-vmstate k))))))

(define CC    (^ (vm d)  (%pushv! (%makecc vm d) vm)))

(define (%decode instruction)
  (values (car instruction)
          (cdr instruction)))

(define (%opname->opfn opname)
  (case opname
    ((NOP) NOP)
    ((HALT) HALT)
    ((CONST) CONST)
    ((PRIM) PRIM)
    ((LREF) LREF)
    ((LSET) LSET)
    ((GREF) GREF)
    ((GSET) GSET)
    ((GO) GO)
    ((FGO) FGO)
    ((TGO) TGO)
    ((SAVE) SAVE)
    ((CALL) CALL)
    ((RETURN) RETURN)
    ((SETCC) SETCC)
    ((CC) CC)
    (else (error (str "Unrecognized VM op name: " opname)))))

(define (%opfn->opname opfn)
  (cond 
   ((eq? opfn NOP) 'NOP)
   ((eq? opfn HALT) 'HALT)
   ((eq? opfn CONST) 'CONST)
   ((eq? opfn PRIM) 'PRIM)
   ((eq? opfn LREF) 'LREF)
   ((eq? opfn LSET) 'LSET)
   ((eq? opfn GREF) 'GREF)
   ((eq? opfn GSET) 'GSET)
   ((eq? opfn GO) 'GO)
   ((eq? opfn FGO) 'FGO)
   ((eq? opfn TGO) 'TGO)
   ((eq? opfn SAVE) 'SAVE)
   ((eq? opfn CALL) 'CALL)
   ((eq? opfn RETURN) 'RETURN)
   ((eq? opfn SETCC) 'SETCC)
   ((eq? opfn CC) 'CC)
   (else (error (str "Unrecognized VM op name: " opname)))))
