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

(define (env:add-binding env var val)
  (cons (cons var val)
        env))
