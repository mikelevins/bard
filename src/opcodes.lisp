;;;; ***********************************************************************
;;;;
;;;; Name:          opcodes.lisp
;;;; Project:       the bard programming lnaguage
;;;; Purpose:       define the vm opcodes
;;;; Author:        mikel evins
;;;; Copyright:     2025 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard)

(defparameter +opcodes+
  '(LVAR
    LSET
    GVAR
    GSET
    POP
    CONST

    ;; Branching instructions:
    JUMP
    FJUMP
    TJUMP

    ;; Function call/return instructions:
    SAVE
    RETURN
    CALLJ
    ARGS
    ARGS.
    FN
    PRIM

    ;; Continuation instructions:
    SET-CC
    CC

    ;; Nullary operations:
    BARD-READ
    NEWLINE

    ;; Unary operations:
    CAR
    CDR
    CADR
    EOF-OBJECT?
    NOT
    LIST1
    COMPILER
    DISPLAY
    WRITE
    RANDOM

    ;; Binary operations:
    +
    -
    *
    /
    <
    >
    <=
    >=
    /=
    =
    CONS
    LIST2
    NAME!
    EQ
    EQUAL
    EQL

    ;; Ternary operations:
    LIST3

    ;; Constants:
    T
    NIL
    -1
    0
    1
    2

    ;; Other:
    HALT))

#+repl (length +opcodes+)
