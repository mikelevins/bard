;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          macro.scm
;;;; Project:       Bard VM
;;;; Purpose:       implementation of macros
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(define $macroexpanders (make-table test: eq?))

(define (%macro-form? expr)
  (table-ref $macroexpanders (car expr) #f))

(define (%macroexpand expr)
  ((table-ref $macroexpanders (car expr) #f) expr))

;;; ---------------------------------------------------------------------
;;; macro definitions
;;; ---------------------------------------------------------------------

(table-set! $macroexpanders 'set!
            (lambda (expr) `((setter ,(cadr expr)) ,(caddr expr))))


