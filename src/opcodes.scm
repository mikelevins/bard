;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          opcodes.scm
;;;; Project:       bard
;;;; Purpose:       values of bard vm opcodes
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

(define $op_NOOP #x00)
(define $op_HALT #x01)
(define $op_RETURN #x02)
(define $op_CONSTANT #x03)
(define $op_TRUE #x04)
(define $op_FALSE #x05)
(define $op_MINUSONE #x06)
(define $op_ZERO #x07)
(define $op_ONE #x08)
(define $op_TWO #x09)
(define $op_UNDEFINED #x0A)
(define $op_NOTHING #x0B)
(define $op_LVAR #x0C)
(define $op_MVAR #x0D)
