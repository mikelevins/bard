;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          instruction.scm
;;;; Project:       Bard
;;;; Purpose:       Bard VM instruction format 
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(define (instruction op . args)
  `(op ,@args))

(define (op instr)
  (car instr))

(define (op-set! instr new-op)
  (set-car! instr new-op))

(define (args instr)
  (cdr instr))

(define (arg1 instr)
  (list-ref instr 1))

(define (arg2 instr)
  (list-ref instr 2))

(define (arg3 instr)
  (list-ref instr 3))

