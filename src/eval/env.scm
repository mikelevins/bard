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

(define (%symbol< s1 s2)
  (string<? (symbol->string s1)
           (symbol->string s2)))

(define $environment-wt-type (make-wt-tree-type %symbol<))
(define %null-environment #f)
(let ((_null-env  (make-wt-tree $environment-wt-type)))
  (set! %null-environment (lambda () _null-env)))

(define (%add-binding env var val)
  (wt-tree/add env var val))

(define (%extend-environment env plist)
  (if (null? plist)
      env
      (if (null? (cdr plist))
          (error "Odd number of arguments to extend-environment" plist)
          (let* ((var (car plist))
                 (val (cadr plist)))
            (%extend-environment (%add-binding env var val) (cddr plist))))))

(define (%lookup-variable-value env var)
  (wt-tree/lookup env var (%undefined)))

(define (%set-variable! env var val)
  (wt-tree/add! env var val))

