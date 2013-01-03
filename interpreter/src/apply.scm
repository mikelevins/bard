;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          apply.scm
;;;; Project:       Bard
;;;; Purpose:       the apply function
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(define (%apply-keyed-collection op args)
  (if (= 1 (length args))
      (cond
       ((null? op) %nil)
       ((string? op)(string-ref op (car args)))
       ((list? op)(list-ref op (car args)))
       ((alist-table? op)(alist-table-get op (car args)))
       (else (error (str "Not an applicable object: " op))))
      (error (str "Too many arguments: " args))))

(define (%apply op args)
  (cond
   ((%keyed-collection? op)(%apply-keyed-collection op args))
   ((function? op)(apply (function-proc op) args))
   ((interpreted-method-instance? op)(apply (interpreted-method-proc op) args))
   ((procedure? op)(apply op (%bard-list->cons args)))
   (else (error (str "not an applicable object: " op "; args: " args)))))

(define %funcall (lambda (fn . args)(%apply fn args)))
