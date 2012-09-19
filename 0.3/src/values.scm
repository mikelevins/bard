;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          values.scm
;;;; Project:       Bard
;;;; Purpose:       Representation of Bard values 
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(define (false? x)(eqv? x #f))
(define (nothing? x)(eqv? x '()))
(define (undefined? x)(eqv? x #!unbound))

(define (logical-false? x)
  (or (false? x)
      (nothing? x)
      (undefined? x)))

(define (logical-true? x)
  (not (logical-false? x)))

