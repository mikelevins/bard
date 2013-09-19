;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          bard.scm
;;;; Project:       Bard
;;;; Purpose:       paip-based compiler/vm implementation experiment
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

(define (instruction-not-implemented name)
  (error (string-append "Instruction " (object-to-string name) " not yet implemented")))

(define (machine fn)
  (let* ((code (fn-code fn))
         (pc 0)
         (env '())
         (stack '())
         (n-args 0)
         (instr nil))
    (let loop ()
       (set! instr (list-ref code pc))
       (set! pc (+ 1 pc))
       (case (opcode instr)
         
         ;; Variable/stack manipulation instructions:
         ((LREF) (instruction-not-implemented 'LREF))
         ((LSET) (instruction-not-implemented 'LSET))
         ((GREF) (instruction-not-implemented 'GREF))
         ((GSET) (instruction-not-implemented 'GSET))
         ((POP) (instruction-not-implemented 'POP))
         ((CONST) (instruction-not-implemented 'CONST))
         
         ;; Branching instructions:
         ((JUMP) (instruction-not-implemented 'JUMP))
         ((FJUMP) (instruction-not-implemented 'FJUMP))
         ((TJUMP) (instruction-not-implemented 'TJUMP))
         
         ;; Function call/return instructions:
         ((SAVE) (instruction-not-implemented 'SAVE))
         ((RETURN) (instruction-not-implemented 'RETURN))
         ((CALL) (instruction-not-implemented 'CALLJ))
         ((ARGS) (instruction-not-implemented 'ARGS))
         ((ARGS.) (instruction-not-implemented 'ARGS.))
         ((FN) (instruction-not-implemented 'FN))
         ((PRIM) (instruction-not-implemented 'PRIM))
         
         ;; Continuation instructions:
         ((SET-CC) (instruction-not-implemented 'SET-CC))
         ((CC) (instruction-not-implemented 'CC))
         
         ;; Nullary operations:
         ((BARD-READ NEWLINE) (instruction-not-implemented '(BARD-READ NEWLINE)))
         
         ;; Unary operations:
         ((CAR CDR CADR NOT LIST1 COMPILER DISPLAY WRITE RANDOM)
          (instruction-not-implemented '(CAR CDR CADR NOT LIST1 COMPILER DISPLAY WRITE RANDOM)))
         
         ;; Binary operations:
         ((+ - * / < > <= >= /= = CONS LIST2 NAME! EQ EQUAL EQL)
          (instruction-not-implemented '(+ - * / < > <= >= /= = CONS LIST2 NAME! EQ EQUAL EQL)))
         
         ;; Ternary operations:
         ((LIST3) (instruction-not-implemented 'LIST3))
         
         ;; Constants:
         ((T NIL -1 0 1 2) (instruction-not-implemented '(T NIL -1 0 1 2)))
         
         ;; Other:
         ((HALT) (instruction-not-implemented 'HALT))
         (else (error "Unknown opcode: ~a" instr))))))
