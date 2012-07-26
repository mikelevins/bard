;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          special.scm
;;;; Project:       Bard
;;;; Purpose:       bard special forms
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(define $special-forms-table (make-table test: eq?))

(define (%defspecial nm eval-fn)
  (table-set! $special-forms-table nm eval-fn))

(define (%special-compiler nm)
  (table-ref $special-forms-table nm #f))

(define (%special-form? expr)
  (and (table-ref $special-forms-table (%car expr) #f)
       #t))

(define (%compile-special-form expr env)
  (let* ((op (%car expr))
         (compiler (table-ref $special-forms-table op #f)))
    (if compiler
        (compiler expr env)
        (error (string-append "unrecognized special form" (%as-string (%car expr)))))))

