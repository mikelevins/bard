;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          environments.scm
;;;; Project:       bard
;;;; Purpose:       representation of variable environments
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

(define (env:make-environment)
  '())

(define (env:extend-environment env var val)
  (cons (cons var val)
        env))

(define (env:get-binding nm env)
  (assq nm env))

