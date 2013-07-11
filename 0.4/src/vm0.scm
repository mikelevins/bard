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
;;; running the vm
;;; ----------------------------------------------------------------------

(define $haltfn #f) ; set by vm startup

(define (vmrun-simple program #!key (step #f))
  (call/cc
   (lambda (exit)
     (set! $haltfn (make-halt-function exit))
     (let ((fn (load-program program))
           (env (default-env))
           (globals (default-globals)))
       (let loop ((code (program-code fn))
                  (pc 0)
                  (vals (make-stack 128))
                  (stack (make-stack 1024)))
         (let* ((instr (code-ref code pc))
                (opcode (instruction-opcode instr)))
           (case instr
             ;; HALT
             ((0) ($haltfn code pc vals nvals env globals))
             ;; LREF
             ((1)(let ((i (%arg1 instr))
                       (j (%arg2 instr)))
                   (stack-push! (%lref env i j) vals)
                   (set! pc (1+ pc))))
             ;; LSET
             ((2)(let ((i (%arg1 instr))
                       (j (%arg2 instr))
                       (v (stack-pop! vals)))
                   (%lset! env i j v)
                   (set! pc (1+ pc))))
             ;; GREF
             ((3)(let ((g (%arg1 instr)))
                   (stack-push! (%gref g) vals)
                   (set! pc (1+ pc))))
             ;; GSET
             ((4)(let ((g (%arg1 instr))
                       (v (stack-pop! vals)))
                   (%gset! globals g v)
                   (set! pc (1+ pc))))
             ;; CONST
             ((6) (begin (stack-push! (%arg1 instr) vals)
                         (set! pc (1+ pc)))) 
             ;; JUMP
             ((7) (set! pc (%arg1 instr))) 
             ;; FJUMP
             ((8) (let ((v (stack-pop! vals)))
                    (if (%false? v)
                        (set! pc (%arg1 instr))
                        (set! pc (1+ pc))))) 
             ;; TJUMP
             ((9) (let ((v (stack-pop! vals)))
                    (if (%true? v)
                        (set! pc (%arg1 instr))
                        (set! pc (1+ pc)))))
             ;; SAVE
             ((10) (begin (stack-push! (%make-saved-state (%arg1 instr) env fn code stack)
                                       stack)
                          (set! pc (1+ pc))))
             ;; RETURN
             ((11) (let ((saved (stack-pop! stack)))
                     (set! pc (saved-state-pc saved))
                     (set! env (saved-state-env saved))
                     (set! fn (saved-state-fn saved))
                     (set! code (saved-state-code saved))))
             ;; CALLJ
             ((12) (let* ((f (stack-pop! stack))
                          (fcode (procedure-code f))
                          (nargs (%arg1 instr))
                          (fenv (%extend-env env (procedure-params f) (stack-take nargs))))
                     (set! pc 0)
                     (set! code fcode)
                     (set! env fenv)))
             ;; ARGS
             ((13) )
             ;; ARGS.
             ((14) )
             ;; FN
             ((15) )
             ;; PRIM
             ((16) )
             ;; SETCC
             ((17) )
             ;; CC
             ((18) )
             ;; nullary: VMREAD NEWLINE
             ((19) )
             ;; unary: CAR CDR NOT LIST1 COMPILE DISPLAY WRITE RANDOM
             ((20) )
             ;; binary: ADD SUB MUL DIV LT GT LTE GTE NEQ EQ EQL EQUAL CONS LIST2
             ((21) )
             ;; ternary: LIST3
             ((22) )
             ;; constants: TRUE FALSE NIL ONE- ZERO ONE TWO
             ((23) ))))))))

(define (vmrun program #!key (step #f))
  (if step
      (vmrun-step program)
      (vmrun-simple program)))
