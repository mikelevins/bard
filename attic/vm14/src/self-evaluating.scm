;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          self-evaluating.scm
;;;; Project:       Bard
;;;; Purpose:       self-evaluating forms
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(define (%self-evaluating? expr) 
  (or (null? expr)
      (boolean? expr)
      (number? expr)
      (keyword? expr)
      (string? expr)))

(define (%compile-self-evaluating expr env) 
  (%gen 'CONST expr))

