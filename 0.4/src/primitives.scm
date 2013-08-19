;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          primitives.scm
;;;; Project:       Bard
;;;; Purpose:       implementation and representation of vm primitives
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

(declare (standard-bindings)
         (extended-bindings)
         (inline)
         (inline-primitives))

;;; ----------------------------------------------------------------------
;;; vm primitives
;;; ----------------------------------------------------------------------

(define-structure primitive required-parameters rest-parameter function)

(define *primitives* (make-table test: eqv?))

(define (defprim pname required-parameters rest-parameter function)
  (let ((prim (make-primitive required-parameters rest-parameter function)))
    (table-set! *primitives* pname prim)
    pname))

(define (get-primitive pname)
  (table-ref *primitives* pname #f))

(defprim 'GNADD 0 #t
  (lambda (state argcount)
    (let* ((args (vmstate-popn! state argcount))
           (sum (apply + args)))
      (vmstate-push! state sum))))

(defprim 'GNSUB 0 #t
  (lambda (state argcount)
    (let* ((args (vmstate-popn! state argcount))
           (sum (apply - args)))
      (vmstate-push! state sum))))

(defprim 'GNMUL 0 #t
  (lambda (state argcount)
    (let* ((args (vmstate-popn! state argcount))
           (prod (apply * args)))
      (vmstate-push! state prod))))
