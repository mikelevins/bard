;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          vm.scm
;;;; Project:       Bard
;;;; Purpose:       the virtual machine
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

(declare (standard-bindings)
         (extended-bindings)
         (inline)
         (inline-primitives))

;;; ----------------------------------------------------------------------
;;; general utilities
;;; ----------------------------------------------------------------------

(define (identity x) x)
(define (1+ n)(+ n 1))

(define (vector-for-each fn vec)
  (let ((len (vector-length vec)))
    (let loop ((i 0))
      (if (< i len)
          (let ((it (vector-ref vec i)))
            (fn it)
            (loop (+ i 1)))
          vec))))

;;; ----------------------------------------------------------------------
;;; vm constants
;;; ----------------------------------------------------------------------

(define +absent+ (string->symbol "+absent+"))
(define +bard-program-sentinel+ 'bard-program)
(define +bard-program-version+ (vector 0 4 0))
(define +bard-vmstate-sentinel+ 'bard-vmstate)
(define +bard-vmstate-version+ (vector 0 4 0))

;;; ----------------------------------------------------------------------
;;; vm value semantics
;;; ----------------------------------------------------------------------

(define (true? x) (not (false? x)))
(define (false? x) (memv x '(() #f)))

(define (present? x) (not (absent? x)))
(define (absent? x) (eqv? x +absent+))

(define (something? x) (not (nothing? x)))
(define (nothing? x) (null? x))

;;; ----------------------------------------------------------------------
;;; vm state data
;;; ----------------------------------------------------------------------

;;; stacks

(define (make-stack)
  '())

;;; environments

(define (default-environment)
  '())

(define (env-ref env varname)
  (assq env varname))

;;; globals

(define (default-globals) 
  (make-table test: eq?))

;;; ----------------------------------------------------------------------
;;; vm programs
;;; ----------------------------------------------------------------------

(define program vector)

(define (make-program instructions) 
  (list->vector instructions))

(define (program-ref program index)
  (vector-ref program index))

;;; ----------------------------------------------------------------------
;;; vmstate structure
;;; ----------------------------------------------------------------------

(define-structure vmstate program pc stack env globals haltfn)

;;; acessing and updating state

(define (vmstate-incpc! s)
  (vmstate-pc-set! s (1+ (vmstate-pc s))))

(define (vmstate-push! s v)
  (vmstate-stack-set! s (cons v (vmstate-stack s))))

(define (vmstate-top s)
  (car (vmstate-stack s)))

(define (vmstate-pop! s)
  (let ((old-stack (vmstate-stack s)))
    (vmstate-stack-set! s (cdr old-stack))
    (car old-stack)))

(define (vmstate-gref s var)
  (table-ref (vmstate-globals s) var +absent+))

(define (vmstate-gset! s var val)
  (table-set! (vmstate-globals s) var val))

(define (vmstate-lref s var)
  (let ((entry (assq (vmstate-env s) var)))
    (or entry +absent+)))

(define (vmstate-lset! s var val)
  (let ((entry (assq (vmstate-env s) var)))
    (if entry
        (begin
          (set-cdr! entry val)
          val)
        (let ((entry (cons var val)))
          (vmstate-env-set! s (cons entry (vmstate-env s)))
          val))))

;;; ----------------------------------------------------------------------
;;; instructions
;;; ----------------------------------------------------------------------

(define instruction list)

(define (make-instruction op-n-args)
  (map identity op-n-args))

(define (instruction-opcode instruction)
  (car instruction))

(define (instruction-args instruction)
  (cdr instruction))

;;; instruction argument lists

(define (arg1 instruction)
  (list-ref (instruction-args instruction) 0))

(define (arg2 instruction)
  (list-ref (instruction-args instruction) 1))

(define (arg3 instruction)
  (list-ref (instruction-args instruction) 2))

;;; ----------------------------------------------------------------------
;;; fns
;;; ----------------------------------------------------------------------

(define-structure fn args restarg body)

;;; ----------------------------------------------------------------------
;;; continuations
;;; ----------------------------------------------------------------------

(define-structure continuation pc stack env)

(define (vmstate-continue! state cc)
  (vmstate-pc-set! (continuation-pc cc))
  (vmstate-stack-set! (continuation-stack cc))
  (vmstate-env-set! (continuation-env cc))
  state)

;;; ----------------------------------------------------------------------
;;; vm operators
;;; ----------------------------------------------------------------------

(define *opnames* (make-table test: eqv?))

(define (defopname code name)
  (table-set! *opnames* code name))

(define (opname opcode)
  (table-ref *opnames* opcode))

(define HALT   0)(defopname HALT 'HALT)
(define CONST  1)(defopname CONST 'CONST)
(define LREF   2)(defopname LREF 'LREF)
(define LSET   3)(defopname LSET 'LSET)
(define GREF   4)(defopname GREF 'GREF)
(define GSET   5)(defopname GSET 'GSET)
(define GO     6)(defopname GO 'GO)
(define TGO    7)(defopname TGO 'TGO)
(define FGO    8)(defopname FGO 'FGO)
(define FN     9)(defopname FN 'FN)
(define CC    10)(defopname CC 'CC)
(define SETCC 11)(defopname SETCC 'SETCC)
(define NARGS 12)(defopname NARGS 'NARGS)
(define ARGS. 13)(defopname ARGS. 'ARGS.)
(define PRIM  14)(defopname PRIM 'PRIM)

(define *bard-vm-operators* (make-table test: eqv?))

(define (defop opcode opfn)
  (table-set! *bard-vm-operators* opcode opfn))

(define (vmoperator opcode)
  (table-ref *bard-vm-operators* opcode))

;;; operator definitions

(defop HALT 
  (lambda (instruction state)
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
      (vmstate-push! state (make-fn args restarg body)))))

;;; ----------------------------------------------------------------------
;;; executing instructions
;;; ----------------------------------------------------------------------

(define (execute-instruction opcode instruction state)
  (let ((op (vmoperator opcode)))
    (op instruction state)))

;;; ----------------------------------------------------------------------
;;; vm main loop
;;; ----------------------------------------------------------------------

(define (vmexec state)
  (let* ((instruction (program-ref (vmstate-program state)
                                   (vmstate-pc state)))
         (opcode (instruction-opcode instruction))
         (state* (execute-instruction opcode instruction state)))
    (vmexec state*)))

(define (vmstart vmstate)
  (call/cc 
   (lambda (exitfn)
     (vmstate-haltfn-set! vmstate exitfn)
     (vmexec vmstate))))

(define (vmrun program)
  (let ((state (make-vmstate program 
                             0
                             (make-stack)
                             (default-environment)
                             (default-globals)
                             #f)))
    (vmstart state)))

;;; ----------------------------------------------------------------------
;;; tools and diagnostics
;;; ----------------------------------------------------------------------

(define (display-instruction ins)
  (let* ((opc (instruction-opcode ins))
         (nm (opname opc)))
    (display nm)
    (for-each (lambda (arg)
                (display " ")
                (display arg))
              (instruction-args ins))))

(define (showvm state)
  (newline)
  (display "Bard VM 0.40")(newline)
  (display "          pc: ")(display (vmstate-pc state))(newline)
  (display " instruction: ")(display-instruction (program-ref (vmstate-program state) (vmstate-pc state)))(newline)
  (display "       stack: ")
  (for-each (lambda (item)
              (newline)
              (display "          ")
              (display item))
            (vmstate-stack state))
  (newline)
  (display " environment: ")
  (for-each (lambda (binding)
              (newline)
              (display "          ")
              (display (car binding))
              (display ": ")
              (display (cdr binding)))
            (vmstate-stack state))
  (newline)
  (display "     globals: ")
  (table-for-each (lambda (key val)
                    (newline)
                    (display "          ")
                    (display key)
                    (display ": ")
                    (display val))
                  (vmstate-globals state))
  (newline)
  (display "     program: ")
  (vector-for-each (lambda (instr)
                     (newline)
                     (display "          ")
                     (display-instruction instr))
                   (vmstate-program state)) 
  (newline))

;;; (define $pgm (make-program (list (make-instruction `(,CONST 0))(make-instruction `(,HALT)))))
;;; (define $vm (make-vmstate $pgm 0 (make-stack)(default-environment)(default-globals) #f))
;;; (showvm $vm)
