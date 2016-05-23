;;;; ***********************************************************************
;;;;
;;;; Name:          globals.scm
;;;; Project:       Bard
;;;; Purpose:       representation of bard's lexical environments
;;;; Author:        mikel evins
;;;; Copyright:     2016 by mikel evins
;;;;
;;;; ***********************************************************************


(define (%null-environment) '())

(define (%add-binding env var val)
  (cons (cons var val) env))

(define (%lookup-variable-value env var)
  (let ((binding (assq var env)))
    (if binding (cdr binding) #!unbound)))

(define (%set-variable! var val env)
  (let ((binding (assq var env)))
    (if binding
        (begin
          (set-cdr! binding val)
          val)
        (let ((global-val (%global-value var)))
          (if (%defined? global-val)
              (begin
                (%defglobal var val)
                val)
              (error (string-append "Undefined variable: " (symbol->string var))))))))

(define (%bound? varname #!optional (env (%null-environment)))
  (if (assq varname env)
      #t
      (%globally-bound? varname)))
