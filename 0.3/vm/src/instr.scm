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
;;; instruction definitions
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

;;; ---------------------------------------------------------------------
;;; the instructions
;;; ---------------------------------------------------------------------

(defop NOP    (vm)   vm)
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
(defop CALL   (vm n) (%callfn vm n))
(defop RETURN (vm)   (%return vm))
(defop CC     (vm d) (%pushv! (%makecc vm d) vm))
(defop SETCC  (vm)   (%setcc! vm))
