;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          codevector.scm
;;;; Project:       Bard
;;;; Purpose:       representation of vm code
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

(declare (standard-bindings)
         (extended-bindings)
         (inline)
         (inline-primitives))

;;; ----------------------------------------------------------------------
;;; 
;;; ----------------------------------------------------------------------

(define-structure codevector code)

(define (codevector-length cvec)
  (vector-length (codevector-code cvec)))

(define (codevector-ref cvec i)
  (vector-ref (codevector-code cvec) i))

