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

(declare (extended-bindings))

(define (%apply op args)
  (cond
   ((function? op)(apply (function-proc op) args))
   ((interpreted-method-instance? op)(apply (interpreted-method-proc op) args))
   ((primitive-instance? op)(apply (primitive-proc op) args))
   ((procedure? op)(apply op args))
   ((list? op)(if (< -1 args (length op))
                  (list-ref op args)
                  '()))
   ((pair? op)(let ((key args))
                (cond ((eq? key 'left)(car op))
                      ((eq? key 'right)(cdr op))
                      (else (error (str "invalid pair key: " (%as-string key)))))))
   ((string? op)(if (< -1 args (string-length op))
                    (string-ref op args)
                    '()))
   ((alist-table-instance? op)(alist-table-get op args))
   (else (error (str "not an applicable object: " op "; args: " args)))))

(define %funcall (lambda (fn . args)(%apply fn args)))

;;; (%eval (bard:read-from-string "(define method (foo x) (* x x))") (%null-environment))
;;; (%eval (bard:read-from-string "(foo 3)") (%null-environment))
;;; (%eval (bard:read-from-string "(define method (foo x) :with ((x <string>)) (pair x x))") (%null-environment))
;;; (%eval (bard:read-from-string "(foo 3)") (%null-environment))
;;; (%eval '(foo 3) (%null-environment))
;;; (define $with (%eval (bard:read-from-string ":with") (%null-environment)))
