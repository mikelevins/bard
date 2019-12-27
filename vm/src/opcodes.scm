;;;; ***********************************************************************
;;;;
;;;; Name:          opcodes.scm
;;;; Project:       Bard VM
;;;; Purpose:       opcode definitions
;;;; Author:        mikel evins
;;;; Copyright:     2019 by mikel evins
;;;;
;;;; ***********************************************************************


;;; machine control
(define HALT 0)

;;; variables and the stack
(define LVAR 11)
(define LSET 12)
(define GVAR 13)
(define GSET 14)
(define POP 15)
(define CONST 16)

;;; branches
(define JUMP 21)
(define FJUMP 22)
(define TJUMP 23)

;;; function calling
(define SAVE 31)
(define RETURN 32)
(define CALLJ 33)
(define ARGS 34)
(define ARGS&REST 35)
(define FN 36)
(define PRIM 37)

;;; continuations
(define SETCC 38)
(define CC 39)

;;; nullary builtins
(define BARD-READ 41)
(define NEWLINE 42)

;;; unary builtins
(define CAR 51)
(define CDR 52)
(define CADR 53)
(define NOT 54)
(define LIST1 55)
(define COMPILER 56)
(define DISPLAY 57)
(define WRITE 58)
(define RANDOM 59)

;;; binary builtins
(define ADD 61)
(define SUB 62)
(define MUL 63)
(define DIV 64)
(define LT 71)
(define GT 72)
(define LTE 73)
(define GTE 74)
(define EQ 75)
(define EQL 76)
(define EQUAL 77)
(define CONS 81)
(define LIST2 82)
(define NAME! 83)

;;; ternary builtins
(define LIST3 91)

;;; constants
(define TRUE 101)
(define FALSE 102)
(define NIL 103)
(define MINUS-ONE 104)
(define ZERO 105)
(define ONE 106)
(define TWO 107)
