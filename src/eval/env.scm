;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          env.scm
;;;; Project:       Bard
;;;; Purpose:       variable-binding environments
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

;;; ---------------------------------------------------------------------
;;; global environment
;;; ---------------------------------------------------------------------

(define (%global-variables)
  (make-table test: eq?))

(define $bard-global-variables #f)

(define (%defglobal var val)
  (table-set! $bard-global-variables var val)
  var)

(define (%global-value var)
  (table-ref $bard-global-variables var (%undefined)))

;;; ---------------------------------------------------------------------
;;; lexical environments
;;; ---------------------------------------------------------------------

(define (%null-environment) '())

(define (%add-binding env var val)
  (cons (cons var val) env))

(define (%extend-environment env plist)
  (if (null? plist)
      env
      (if (null? (cdr plist))
          (error "Odd number of arguments to extend-environment" plist)
          (let* ((var (car plist))
                 (val (cadr plist)))
            (%extend-environment (%add-binding env var val) (cddr plist))))))

(define (%lookup-variable-value env var)
  (let ((binding (assq var env)))
    (if binding (cdr binding) (%undefined))))

(define (%set-variable! env var val)
  (let ((binding (assq var env)))
    (if binding
        (begin
          (set-cdr! binding val)
          val)
        (error (string-append "Undefined variable: " (symbol->string var))))))

