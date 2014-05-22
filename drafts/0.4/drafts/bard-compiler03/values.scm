;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          values.scm
;;;; Project:       Bard
;;;; Purpose:       bard value semantics
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

(define (true? x) (not (false? x)))
(define (false? x) (memv x '(() #f)))

(define (present? x) (not (absent? x)))
(define (absent? x) (eqv? x +absent+))

(define (something? x) (not (nothing? x)))
(define (nothing? x) (null? x))
