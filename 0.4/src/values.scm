;;;; ***********************************************************************
;;;;
;;;; Name:          values.scm
;;;; Project:       Bard
;;;; Purpose:       representation of Bard values
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(declare (extended-bindings))

;;; ----------------------------------------------------------------------
;;; nothing
;;; ----------------------------------------------------------------------

(define (nothing) '())
(define (nothing? x)(eq? x '()))
(define (something? x)(not (nothing? x)))

;;; ----------------------------------------------------------------------
;;; booleans
;;; ----------------------------------------------------------------------

(define (true) #t)
(define (false) #f)
(define (false? x)(or (not x)(null? x)))
(define (true? x)(not (false? x)))

