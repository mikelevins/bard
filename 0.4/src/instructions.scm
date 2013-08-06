;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          instructions.scm
;;;; Project:       Bard
;;;; Purpose:       implementation and representation of vm instructions
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

(declare (standard-bindings)
         (extended-bindings)
         (inline)
         (inline-primitives))

;;; ----------------------------------------------------------------------
;;; representation of instructions
;;; ----------------------------------------------------------------------

(define instruction list)

(define (make-instruction op-n-args)
  (map identity op-n-args))

(define (instruction-opcode instruction)
  (car instruction))

(define (instruction-args instruction)
  (cdr instruction))

(define (arg1 instruction)
  (list-ref (instruction-args instruction) 0))

(define (arg2 instruction)
  (list-ref (instruction-args instruction) 1))

(define (arg3 instruction)
  (list-ref (instruction-args instruction) 2))

;;; ----------------------------------------------------------------------
;;; implementation of instructions
;;; ----------------------------------------------------------------------

;;; relating opcodes to operators

(define *opnames* (make-table test: eqv?))
(define *opcodes* (make-table test: eqv?))

(define (defopname code name)
  (table-set! *opnames* code name)
  (table-set! *opcodes* name code))

(define (opname opcode)
  (table-ref *opnames* opcode))

(define *bard-vm-operators* (make-table test: eqv?))

(define (opcode opname)
  (table-ref *opcodes* opname))

(define (defop opcode opfn)
  (table-set! *bard-vm-operators* opcode opfn))

(define (vmoperator opcode)
  (table-ref *bard-vm-operators* opcode))

;;; defining opcodes and opnames

(define HALT      0)  (defopname HALT 'HALT)
(define CONST     1)  (defopname CONST 'CONST)
(define LREF      2)  (defopname LREF 'LREF)
(define LSET      3)  (defopname LSET 'LSET)
(define GREF      4)  (defopname GREF 'GREF)
(define GSET      5)  (defopname GSET 'GSET)
(define GO        6)  (defopname GO 'GO)
(define TGO       7)  (defopname TGO 'TGO)
(define FGO       8)  (defopname FGO 'FGO)
(define PRIM      9)  (defopname PRIM 'PRIM)
(define SAVE     10)  (defopname SAVE 'SAVE)
(define RETURN   12)  (defopname RETURN 'RETURN)
(define APPLY    13)  (defopname APPLY 'APPLY)
(define CC       14)  (defopname CC 'CC) ; "capture continuation"
(define SETCC    15)  (defopname SETCC 'SETCC) ; "set captured continuation" - for arbitrary transfers of control

;;; defining operations

(defop HALT 
  (lambda (instruction state)
    (vmstate-incpc! state)
    (let ((exit (vmstate-haltfn state)))
      (exit state))))

(defop CONST 
  (lambda (instruction state)
    (vmstate-push! state (arg1 instruction))
    (vmstate-incpc! state)
    state))

(defop LREF 
  (lambda (instruction state)
    (let ((entry (vmstate-lref state (arg1 instruction))))
      (if entry
          (vmstate-push! state (cdr entry))
          (vmstate-push! state +absent+))
      (vmstate-incpc! state)
      state)))

(defop LSET 
  (lambda (instruction state)
    (let* ((varname (arg1 instruction))
           (var (env-ref varname (vmstate-env state)))
           (val (vmstate-pop! state)))
      (if var
          (set-cdr! var val)
          (vmstate-env-set! state
                            (cons (cons varname val)
                                  (vmstate-env state)))))
    (vmstate-incpc! state)
    state))

(defop GREF 
  (lambda (instruction state)
    (vmstate-push! state (vmstate-gref state (arg1 instruction)))
    (vmstate-incpc! state)
    state))

(defop GSET 
  (lambda (instruction state)
    (let* ((varname (arg1 instruction))
           (val (vmstate-pop! state)))
      (table-set! (vmstate-globals state) varname val))
    (vmstate-incpc! state)
    state))

(defop GO 
  (lambda (instruction state)
    (vmstate-pc-set! state (arg1 instruction))
    state))

(defop TGO 
  (lambda (instruction state)
    (if (true? (vmstate-pop! state))
        (vmstate-pc-set! state (arg1 instruction))
        (vmstate-incpc! state))
    state))

(defop FGO 
  (lambda (instruction state)
    (if (false? (vmstate-pop! state))
        (vmstate-pc-set! state (arg1 instruction))
        (vmstate-incpc! state))
    state))

(defop FN 
  (lambda (instruction state)
    (let ((args (arg1 instruction))
          (restarg (arg2 instruction))
          (body (arg3 instruction)))
      (vmstate-push! state (make-fn args restarg body))
      (vmstate-incpc! state)
      state)))

(defop PRIM
  (lambda (instruction state)
    (let* ((pname (arg1 instruction))
           (prim (get-primitive pname))
           (pfun (primitive-function prim)))
      (pfun state)
      (vmstate-incpc! state)
      state)))

(defop SAVE
  (lambda (instruction state)
    (let ((rr (make-return (vmstate-fn state)
                           (vmstate-pc state)
                           (vmstate-stack state)
                           (vmstate-env state))))
      (vmstate-push! state rr)
      (vmstate-incpc! state)
      state)))

(defop APPLY
  (lambda (instruction state)
    (vmstate-apply! state)    
    state))

(defop RETURN
  (lambda (instruction state)
    (let ((rr (vmstate-pop-bottom! state)))
      (vmstate-return! state rr)
      state)))

(defop CC
  (lambda (instruction state)
    (vmstate-push! state (make-continuation (vmstate-stack state)))
    (vmstate-incpc! state)
    state))

(defop SETCC
  (lambda (instruction state)
    (let ((cc (vmstate-pop! state)))
      (vmstate-setcc! state cc)
      (vmstate-incpc! state)
      state)))





