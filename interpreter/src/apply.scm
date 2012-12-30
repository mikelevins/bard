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
  (if (= 1 (%length args))
      (cond
       ((%null? op) %nil)
       ((string? op)(string-ref op (%car args)))
       ((%list? op)(%list-ref op (%car args)))
       ((%table? op)(%table-get op (%car args)))
       (else (error (string-append "Not an applicable object: " (%as-string op)))))
      (error (string-append "Too many arguments: " (%as-string args)))))

(define (%apply op args)
  (cond
   ((%keyed-collection? op)(%apply-keyed-collection op args))
   ((%callable? op)(apply (%callable-function op) args))
   ((procedure? op)(apply op (%bard-list->cons args)))
   (else (error (string-append "not an applicable object: " (object->string op) "; args: " (object->string args))))))

(define %funcall (lambda (fn . args)(%apply fn args)))
