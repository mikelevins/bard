;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          main.scm
;;;; Project:       Bard
;;;; Purpose:       toplevel entry point for the Bard VM
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

(define $test
  (vector (list LREF 0 0)
          (list CONST 1000000)
          (list GT)
          (list FJUMP 9)
          (list LREF 0 0)
          (list CONST 1)
          (list ADD)
          (list LSET 0 0)
          (list JUMP 0)
          (list HALT)))

(vmrun $test)
