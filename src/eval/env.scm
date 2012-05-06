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

(define (%global-environment)
  (make-table test: eq?))

(define $bard-global-environment #f)

(define (%defglobal var val)
  (table-set! $bard-global-environment var val)
  var)

(define (%global-value var)
  (table-ref $bard-global-environment var (%undefined)))

;;; ---------------------------------------------------------------------
;;; lexical environments
;;; ---------------------------------------------------------------------

(define (%null-environment) '())

(define (%add-binding env var val)
  (cons (cons var val)
        env))

(define (%find-binding env var)
  (assq var env))

(define (%binding-value binding)(cdr binding))

(define (%set-binding-value! binding val)(set-cdr! binding val))

(define (%extend-environment env plist)
  (if (null? plist)
      env
      (if (null? (cdr plist))
          (error "Odd number of arguments to extend-environment" plist)
          (let* ((var (car plist))
                 (val (cadr plist)))
            (%extend-environment (%add-binding env var val) (cddr plist))))))

(define (%set-variable! env var val)
  (let ((binding (%find-binding env var)))
    (if binding
        (%set-binding-value! var val)
        (if (%defined? (%global-value var))
            (%defglobal var val)
            (error "undefined variable " var)))
    val))

