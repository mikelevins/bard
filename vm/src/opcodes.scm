;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          opcodes.scm
;;;; Project:       Bard
;;;; Purpose:       vm opcodes
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

;;; 00 machine control
(define HALT    0)
(define JUMP    1)
(define TJUMP   2)
(define FJUMP   3)
(define CC      4) ; obtain the current continuation, reified as a procedure
(define SETCC   5) ; replace the VM state with the supplied continuation
(define PRIM    6)

;;; 32 values
(define CONST  32)
(define NIL    33)
(define ZERO   34)
(define ONE    35)
(define ONE-   36)
(define TWO    37)

;;; 64 variables
(define LREF   64)
(define LSET   65)
(define MREF   66)
(define MSET   67)
(define MODL   68)
(define ARGS   69) ; move some values to a new env frame

;;; 96 arithmetic and logic
(define GT     96)
(define GTE    97)
(define LT     98)
(define LTE    99)
(define ADD   100)
(define SUB   101)
(define MUL   102)
(define DIV   103)
(define REM   104)
(define EXPT  105)

;;; 128 data manipulations
(define CONS   128)
(define CAR    129)
(define CDR    130)
(define VEC    131)
(define VREF   132)
(define VSET   133)
(define TBL    134)
(define TBLREF 135)
(define TBLSET 136)

;;; 160 system and I/O
(define WRITE 160)
(define READ  161)
(define SAVE  162) ; write the memory image to storage
(define LOAD  163) ; read a memory image from storage
(define SER   164) ; convert a value to serialized form
(define DSER  165) ; convert a bytevector to a Bard value (deserialize it)

;;; 192 network and IPC
(define THIS  192) ; obtain a reference to the actor representing the current process
(define SEND  193)
(define RECV  194)






