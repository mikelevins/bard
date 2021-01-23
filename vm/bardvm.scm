;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          bardvm.scm
;;;; Project:       Bard
;;;; Purpose:       toplevel entry point for the Bard interpreter
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(define-structure vm fn code pc env stack nargs instr)

(define *vm* #f)

(define (%init-vm #!key (fn #f)(code '())(pc 0)(env '())(stack '())(nargs 0)(instr #f))
  (set! *vm* (make-vm fn code pc env stack nargs instr)))

;;; (%init-vm)

;;; ---------------------------------------------------------------------
;;; opcodes
;;; ---------------------------------------------------------------------

;;; stopping the vm
(define _HALT 0)

;;; stack and variables
(define _LVAR 10)
(define _LSET 11)
(define _GVAR 12)
(define _GSET 13)
(define _POP 14)
(define _CONST 15)

;;; jumps and branches
(define _JUMP 20)
(define _FJUMP 21)
(define _TJUMP 22)

;;; calling and returning
(define _SAVE 30)
(define _RETURN 31)
(define _CALLJ 32)
(define _ARGS 33)
(define _ARGS&REST 34)
(define _PRIM 35)

;; functions and continuations
(define _FN 40)
(define _SETCC 41)
(define _CC 42)


;;; ---------------------------------------------------------------------
;;; instruction helpers
;;; ---------------------------------------------------------------------

(define (opcode instr) )
(define (arg1 instr) )

;;; ---------------------------------------------------------------------
;;; env helpers
;;; ---------------------------------------------------------------------

(define (env-get env var) )
(define (env-set! env var val) )

;;; ---------------------------------------------------------------------
;;; vm helpers
;;; ---------------------------------------------------------------------

(define (true? val) )
(define (false? val) )
(define (vm-stack-push! vm val) )
(define (vm-getvar vm varname) )
(define (vm-setvar! vm varname val) )

;;; ---------------------------------------------------------------------
;;; run-vm
;;; ---------------------------------------------------------------------

(define (run-vm vm)
  (let loop ()
    (set-instr! vm (list-ref (vm-code vm) (vm-pc vm)))
    (vm-pc-set! vm (+ 1 (vm-pc vm)))
    (let ((opc (opcode (vm-instr vm))))
      (cond ((= _HALT opc) vm)
            ((= _LVAR opc) (begin (vm-stack-push! vm (env-get (vm-env vm)(arg1 (vm-instr vm))))
                                  (loop)))
            ((= _LSET opc) (begin (env-set! vm
                                            (arg1 (vm-instr vm))
                                            (vm-stack-top vm))
                                  (loop)))
            ((= _GVAR opc) (begin (vm-stack-push! vm (vm-getvar (vm-env vm)(arg1 (vm-instr vm))))
                                  (loop)))
            ((= _GSET opc) (begin (vm-setvar! vm
                                              (arg1 (vm-instr vm))
                                              (vm-stack-top vm))
                                  (loop)))
            ((= _POP opc) (begin (vm-stack-pop! vm) (loop)))
            ((= _CONST opc) (begin (vm-stack-push! vm (arg1 (vm-instr vm)))
                                   (loop)))
            ((= _JUMP opc) (begin (vm-pc-set! vm (arg1 (vm-instr vm)))
                                  (loop)))
            ((= _FJUMP opc) (begin
                              (if (false? (vm-stack-pop! vm))
                                  (vm-pc-set! vm (arg1 (vm-instr vm)))
                                  #f)
                              (loop)))
            ((= _TJUMP opc) (begin
                              (if (true? (vm-stack-pop! vm))
                                  (vm-pc-set! vm (arg1 (vm-instr vm)))
                                  #f)
                              (loop)))
            ((= _SAVE opc) (begin (vm-stack-push! vm
                                                  (make-ret-addr pc: (arg1 (vm-instr vm))
                                                                 fn: (vm-fn vm)
                                                                 env: (vm-env vm)))
                                  (loop)))
            ((= _RETURN opc) (let ((val (vm-stack-pop! vm))
                                   (ret (vm-stack-pop! vm)))
                               (vm-fn-set! vm (ret-addr-fn ret))
                               (vm-code-set! vm (fn-code (ret-addr-fn ret)))
                               (vm-env-set! vm (ret-addr-env ret))
                               (vm-pc-set! vm (ret-addr-pc ret))
                               (vm-stack-push! vm val)
                               (loop)))
            ((= _CALLJ opc) )
            ((= _ARGS opc) )
            ((= _ARGS&REST opc) )
            ((= _PRIM opc) )
            ((= _FN opc) )
            ((= _SETCC opc) )
            ((= _CC opc) )
            (else (error (string-append "Unrecognized opcode: " (number->string opc))))))))
