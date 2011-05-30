;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          instructions.scm
;;;; Project:       bard
;;;; Purpose:       bardvm instructions
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

(define HALT
  (lambda (vm) vm))

(define NOOP
  (lambda (vm)
    (vm:%incpc vm)
    (vm:%next vm)))

(define CONST 
  (lambda (vm k)
    (vm:%push vm k)
    (vm:%incpc vm)
    (vm:%next vm)))


(define $instructions (make-table))

(define (definstruction opcode instruction)
  (table-set! $instructions opcode instruction))

(definstruction op_HALT HALT)
(definstruction op_NOOP NOOP)
(definstruction op_CONST CONST)
