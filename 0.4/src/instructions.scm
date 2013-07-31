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

(define HALT    0)  (defopname HALT 'HALT)
(define CONST   1)  (defopname CONST 'CONST)
(define LREF    2)  (defopname LREF 'LREF)
(define LSET    3)  (defopname LSET 'LSET)
(define GREF    4)  (defopname GREF 'GREF)
(define GSET    5)  (defopname GSET 'GSET)
(define GO      6)  (defopname GO 'GO)
(define TGO     7)  (defopname TGO 'TGO)
(define FGO     8)  (defopname FGO 'FGO)
(define FN      9)  (defopname FN 'FN)
(define CC     10)  (defopname CC 'CC)
(define SETCC  11)  (defopname SETCC 'SETCC)
(define PRIM   12)  (defopname PRIM 'PRIM)
(define SAVE   13)  (defopname SAVE 'SAVE)
(define CALL   14)  (defopname CALL 'CALL)
(define RETURN 15)  (defopname RETURN 'RETURN)

;;; defining operations

(defop HALT 
  (lambda (instruction state)
    (vmstate-incpc! state)
    (let ((exit (vmstate-haltfn state)))
      (exit state))))

(defop CONST 
  (lambda (instruction state)
    (vmstate-stack-push! state (arg1 instruction))
    (vmstate-incpc! state)))

(defop LREF 
  (lambda (instruction state)
    (vmstate-stack-push! state (vmstate-lref state (arg1 instruction)))
    (vmstate-incpc! state)))

(defop LSET 
  (lambda (instruction state)
    (let* ((varname (arg1 instruction))
           (var (env-ref (vmstate-env state) varname))
           (val (vmstate-pop! state)))
      (if var
          (set-cdr! var val)
          (vmstate-env-set! state
                            (cons (cons varname val)
                                  (vmstate-env state)))))
    (vmstate-incpc! state)))

(defop GREF 
  (lambda (instruction state)
    (vmstate-stack-push! state (vmstate-gref state (arg1 instruction)))
    (vmstate-incpc! state)))

(defop GSET 
  (lambda (instruction state)
    (let* ((varname (arg1 instruction))
           (val (vmstate-pop! state)))
      (table-set! (vmstate-globals state) varname val))
    (vmstate-incpc! state)))

(defop GO 
  (lambda (instruction state)
    (vmstate-pc-set! state (arg1 instruction))))

(defop TGO 
  (lambda (instruction state)
    (if (true? (vmstate-pop! state))
        (vmstate-pc-set! state (arg1 instruction))
        (vmstate-incpc! state))))

(defop FGO 
  (lambda (instruction state)
    (if (false? (vmstate-pop! state))
        (vmstate-pc-set! state (arg1 instruction))
        (vmstate-incpc! state))))

(defop FN 
  (lambda (instruction state)
    (let ((args (arg1 instruction))
          (restarg (arg2 instruction))
          (body (arg3 instruction)))
      (vmstate-push! state (make-fn args restarg body))
      (vmstate-incpc! state))))

(defop CC 
  (lambda (instruction state)
    (let ((pc (vmstate-pc state))
          (stack (vmstate-stack state))
          (env (vmstate-env state)))
      (vmstate-push! state (make-continuation pc stack env)))
    (vmstate-incpc! state)))

(defop SETCC
  (lambda (instruction state)
    (let ((cc (vmstate-pop! state)))
      (vmstate-continue! state cc))))

(defop PRIM
  (lambda (instruction state)
    (let* ((pname (arg1 instruction))
           (prim (get-primitive pname))
           (pfun (primitive-function prim)))
      (pfun state)
      (vmstate-incpc! state))))

(defop SAVE
  (lambda (instruction state)
    (let* ((destpc (arg1 instruction))
           (ret (make-return destpc (vmstate-fn state)(vmstate-stack state))))
      (vmstate-push! state ret)
      (vmstate-incpc! state))))

(defop CALL
  (lambda (instruction state)
    (let* ((fn (vmstate-pop! state))
           (args (vmstate-popn! state (vmstate-nvals state)))
           (ambient-env (vmstate-env state))
           (fenv (fn-env fn))
           (params-env (make-fn-env fn args))
           (call-env (merge-environments params-env fenv ambient-env)))
      (vmstate-env-set! call-env)
      (vmstate-function-set! fn)
      (vmstate-pc-set! 0))))

(defop RETURN
  (lambda (instruction state)
    (let* ((vals (vmstate-popn! state (vmstate-nvals state)))
           (ret (vmstate-pop! state)))
      (vmstate-return! state ret vals))))
