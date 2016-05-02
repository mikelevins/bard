;;;; ***********************************************************************
;;;;
;;;; Name:          apply.scm
;;;; Project:       Bard
;;;; Purpose:       the apply function
;;;; Author:        mikel evins
;;;; Copyright:     2016 by mikel evins
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
   ((record-instance? op)(record-ref op args))
   ((tuple-instance? op)(tuple-ref op args))
   (else (error (str "not an applicable object: " op "; args: " args)))))

(define %funcall (lambda (fn . args)(%apply fn args)))
