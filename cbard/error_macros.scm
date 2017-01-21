;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          error_macros.scm
;;;; Project:       Bard
;;;; Purpose:       managing Scheme error reports
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

(define-macro (with-recorded-errors default-val . body)
  (let ((errvar (gensym)))
    `(with-exception-catcher
      ;; error handler
      (lambda (,errvar)(begin (record-error-report! (make-error-report ,errvar)) ,default-val))
      ;; body
      (lambda () (begin (clear-error-reports!) ,@body)))))
