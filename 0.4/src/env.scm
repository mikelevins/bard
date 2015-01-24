;;;; ***********************************************************************
;;;;
;;;; Name:          env.scm
;;;; Project:       Bard
;;;; Purpose:       representation of lexical environments
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(declare (extended-bindings))

(define (env:empty-environment) '())

(define (env:get-binding expr env)
  (assv expr env))

(define (env:binding-value binding)
  (cdr binding))

(define (env:binding-set! binding val)
  (set-cdr! binding val))

(define (env:ref env expr #!optional (default #f))
  (let ((binding (env:get-binding expr env)))
    (if binding
        (cdr binding)
        default)))

(define (env:add-binding env var val)
  (cons (cons var val)
        env))

