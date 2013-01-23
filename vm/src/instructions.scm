;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          instructions.scm
;;;; Project:       Bard
;;;; Purpose:       implementations of VM instructions
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

(declare (gambit-scheme)
         (standard-bindings)
         (extended-bindings)
         (inline)
         (proper-tail-calls)
         (block))

(##include "opmacros.scm")

(define HALT (lambda ()(end)))
(define CONST (lambda (k)(pushv! k)(incpc!)))
(define ZERO (lambda ()(pushv! 0)(incpc!)))
(define ONE (lambda ()(pushv! 1)(incpc!)))
(define NIL (lambda ()(pushv! '())(incpc!)))
(define JUMP (lambda (d)(set! $pc d)))
(define FJUMP (lambda (d)(if (popv!)(incpc!)(set! $pc d))))
(define LREF (lambda (i j)(pushv! (lref i j))(incpc!)))
(define LSET (lambda (i j)(lset! i j (popv!))(incpc!)))
(define GT (lambda ()(pushv! (> (popv!)(popv!)))(incpc!)))
(define ADD (lambda ()(pushv! (+ (popv!)(popv!)))(incpc!)))
(define CONS (lambda ()(pushv! (cons (popv!)(popv!)))(incpc!)))
(define CAR (lambda ()(pushv! (car (popv!)))(incpc!)))
(define CDR (lambda ()(pushv! (cdr (popv!)))(incpc!)))



