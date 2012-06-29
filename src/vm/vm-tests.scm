;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          vm-tests.scm
;;;; Project:       Bard
;;;; Purpose:       testins of the Bard vm implementation
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(include "asm.scm")
;;; (include "/Volumes/ymra/Users/mikel/Projects/bard/bard/src/vm/asm.scm")

#| tests

HALT
----
(vm:run-program (%asm ((HALT))) show: #t)

expected end state:
pc: 1 instr: (HALT) halted: #t stack: () 

LVAR
----
(let* ((env (vm:add-frame (vm:null-env)
                          (vm:make-frame '(5))))
       (code (%asm ((LVAR 0 0)(HALT))))
       (vm (vm:make-vm fun: (vm:make-method code: code env: '())
                       env: env)))
  (vm:run-show vm))

expected end state:
pc: 2 instr: (HALT) halted: #t stack: (5) 

LSET
----
(let* ((env (vm:add-frame (vm:null-env)
                          (vm:make-frame '(5))))
       (code (%asm ((CONST 10)(LSET 0 0)(POP)(LVAR 0 0)(HALT))))
       (vm (vm:make-vm fun: (vm:make-method code: code env: '())
                       env: env)))
  (vm:run-show vm))

expected end state:
pc: 5 instr: (HALT) halted: #t stack: (10) 

GVAR
GSET
POP
CONST
JUMP
FJUMP
TJUMP
SAVE
RETURN
CALLJ
ARGS
ARGS.
METH
PRIM
TRUE
FALSE
MINUSONE
ZERO
ONE
TWO
NIL
CONS
CAR
CDR
EQ

|#


