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
(define SETFN    11)  (defopname SETFN 'SETFN)
(define CONTINUE 12)  (defopname CONTINUE 'CONTINUE)
(define APPLY    13)  (defopname APPLY 'APPLY)

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
    (let* ((dest (arg1 instruction))
           (env (vmstate-env state))
           (stack (vmstate-stack state))
           (cc (make-continuation stack env dest)))
      (vmstate-push! state cc)
      (vmstate-incpc! state)
      state)))

(defop SETFN
  (lambda (instruction state)
    (let* ((fn (vmstate-pop! state))
           (code (fn-code fn)))
      (vmstate-fn-set! state fn)
      (vmstate-code-set! state (fn-code fn))
      (vmstate-incpc! state)
      state)))

(defop ARGS
  (lambda (instruction state)
    (let* ((fn (vmstate-fn state))
           (params (fn-required-parameters fn))
           (paramcount (length params))
           (argcount (vmstate-nvals state))
           (args (vmstate-popn! state argcount))
           (callenv (make-fn-env fn args)))
      (vmstate-env-set! state callenv)
      (vmstate-incpc! state)
      state)))

(defop APPLY
  (lambda (instruction state)
    (let* ((fn (vmstate-pop! state))
           (code (fn-code fn))
           (params (fn-required-parameters fn))
           (paramcount (length params))
           (argcount (vmstate-nvals state))
           (args (vmstate-popn! state argcount))
           (callenv (make-fn-env fn args)))
      (vmstate-fn-set! state fn)
      (vmstate-code-set! state (fn-code fn))
      (vmstate-env-set! state callenv)
      (vmstate-pc-set! state 0)
      state)))

(defop ROTATE
  (lambda (instruction state)
    (let* ((index (arg1 instruction))
           (stack (vmstate-stack state))
           (len (length stack))
           (pivot (- len index))
           (left (take pivot stack))
           (right (drop pivot stack))
           (stack* (append right left)))
      (vmstate-stack-set! state stack*)
      (vmstate-incpc! state)
      state)))

(defop CONTINUE
  (lambda (instruction state)
    (let* ((cc (vmstate-pop! state)))
      (vmstate-continue! state cc)
      state)))


