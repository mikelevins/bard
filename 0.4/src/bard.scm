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

(define (opcode instr)(list-ref instr 0))
(define (arg1 instr)(list-ref instr 1))
(define (arg2 instr)(list-ref instr 2))

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
         ((CALL) (instruction-not-implemented 'CALLJ))
         ((FUNCTION) (instruction-not-implemented 'FUNCTION))
         ((METHOD) (instruction-not-implemented 'METHOD))
         ((PRIM) (instruction-not-implemented 'PRIM))

         ;; Unary operations:
         ((NOT-YET-IMPLEMENTED) (error (string-append "NOT-YET-IMPLEMENTED: " (object->string (arg1 instr)))))
         ((UNDEF) (instruction-not-implemented 'UNDEF))

         ;; Binary operations:
         ((DEF) (instruction-not-implemented 'DEF))
         
         ;; Other:
         ((HALT) (instruction-not-implemented 'HALT))
         (else (error "Unknown opcode: ~a" instr))))))
