;;;; ***********************************************************************
;;;;
;;;; Name:          bardvm.scm
;;;; Project:       Bard
;;;; Purpose:       toplevel entry point for the Bard interpreter
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

;;; ---------------------------------------------------------------------
;;; structures
;;; ---------------------------------------------------------------------

(define-structure ret-addr fn pc env)
(define-structure vm fn code pc env stack nargs instr)

(define *vm* #f)

(define (%init-vm #!key (fn #f)(code '())(pc 0)(env '())(stack '())(nargs 0)(instr #f))
  (set! *vm* (make-vm fn code pc env stack nargs instr)))

;;; (%init-vm)


;;; ---------------------------------------------------------------------
;;; vm helpers
;;; ---------------------------------------------------------------------

(define (true? val) #f)
(define (false? val) #f)
(define (vm-stack-push! vm val) #f)
(define (vm-stack-pop! vm) #f)
(define (vm-env-push! vm frame) #f)
(define (vm-env-pop! vm) #f)
(define (vm-getvar vm varname) #f)
(define (vm-setvar! vm varname val) #f)

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
            
            (else (error (string-append "Unrecognized opcode: " (number->string opc))))))))
