;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          api_error.scm
;;;; Project:       Bard
;;;; Purpose:       Scheme API functions
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

;;; ---------------------------------------------------------------------
;;; ABOUT
;;; ---------------------------------------------------------------------
;;; This file defines Scheme API functions that are called by the C API
;;; functions defined as c-lambdas in bard_c_api.scm

(define $bard-error #f)

(define-macro (reporting-errors . body)
  (let ((errvar (gensym)))
    `(with-exception-catcher
      ;; error handler
      (lambda (,errvar)(begin (set! $bard-error (error->string ,errvar)) #f))
      ;; body
      (lambda () (begin (set! $bard-error #f) ,@body)))))
