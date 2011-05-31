;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          instructions.scm
;;;; Project:       bard
;;;; Purpose:       bardvm instructions
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

(define HALT (lambda (vm) vm))

(define NOOP (lambda (vm)(values)))

(define CONST (lambda (vm k)(vm:%push vm k)))


(define $instructions (make-table))

(define (definstruction opcode instruction)
  (table-set! $instructions opcode instruction))

(definstruction op_HALT HALT)
(definstruction op_NOOP NOOP)
(definstruction op_CONST CONST)


(define (instr:%print-instruction instr)
  (cond
   ((= op_HALT (car instr)) (display "HALT"))
   ((= op_NOOP (car instr)) (display "NOOP"))
   ((= op_CONST (car instr)) (display "CONST ")(bard:print-object (cadr instr)))
   (else (error "unrecognized instruction while printing" instr))))