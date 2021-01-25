;;;; ***********************************************************************
;;;;
;;;; Name:          opcodes.scm
;;;; Project:       Bard
;;;; Purpose:       definition of Bard VM opcodes
;;;; Author:        mikel evins
;;;; Copyright:     2021 by mikel evins
;;;;
;;;; ***********************************************************************

;;; ---------------------------------------------------------------------
;;; opcodes
;;; ---------------------------------------------------------------------

;;; stopping the vm
(define _HALT 0)

;;; stack and variables
(define _LVAR 10)
(define _LSET 11)
(define _GVAR 12)
(define _GSET 13)
(define _POP 14)
(define _CONST 15)

;;; jumps and branches
(define _JUMP 20)
(define _FJUMP 21)
(define _TJUMP 22)

;;; calling and returning
(define _SAVE 30)
(define _RETURN 31)
(define _CALLJ 32)
(define _ARGS 33)
(define _ARGS&REST 34)
(define _PRIM 35)

;; functions and continuations
(define _METH 40)
(define _SETCC 41)
(define _CC 42)
