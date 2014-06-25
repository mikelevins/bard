;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          language.scm
;;;; Project:       bard 0.4
;;;; Purpose:       language definition
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export BardLanguage bard-instance)

(define-private-alias Environment gnu.mapping.Environment)
(define-private-alias Scheme kawa.standard.Scheme)

(define null-environment (make-parameter (Environment:make "null-environment")))

(define %bard-environment (make-parameter #f))
(define (bard-environment)
  (or (%bard-environment)
      (begin
        (%bard-environment (Environment:make "bard-environment" (null-environment)))
        (%bard-environment))))

(define %bard-instance (make-parameter #f))
(define (bard-instance)
  (or (%bard-instance)
      (begin
        (%bard-instance (BardLanguage (bard-environment)))
        (%bard-instance))))

(define-simple-class BardLanguage (Scheme)
  ((*init* env::Environment)(begin (invoke-special kawa.standard.Scheme (this) '*init* env)
                                   (Environment:setCurrent env)
                                   (*:defSntxStFld (this) "^" "kawa.standard.SchemeCompilation" "lambda")))
  ((getInstance) allocation: 'static (bard-instance))
  ((builtin) allocation: 'static (bard-environment)))


