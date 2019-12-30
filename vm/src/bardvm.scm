;;;; ***********************************************************************
;;;;
;;;; Name:          bardvm.scm
;;;; Project:       Bard VM
;;;; Purpose:       the Bard VM main program
;;;; Author:        mikel evins
;;;; Copyright:     2019 by mikel evins
;;;;
;;;; ***********************************************************************

(define-structure vm fn code pc env stack nargs instr halt)

(define (fetch-next-instr! vm)
  (vm-instr-set! vm
                 (vector-ref (vm-code vm)
                             (vm-pc vm))))

(define (inc-pc! vm)(vm-pc-set! vm (+ 1 (vm-pc vm))))

(define (set-pc! vm newpc) (vm-pc-set! vm newpc))

(define (current-opcode vm) (instr-opcode (vm-instr vm)))

(define (stack-push! vm val) #f)
(define (stack-pop! vm) #f)
(define (stack-npop! vm count) #f)
(define (stack-top vm) #f)
(define (env-pop! vm) #f)
(define (env-push! vm frame) #f)
(define (make-env-frame vals) #f)
(define (arg1 vm) #f)
(define (arg2 vm) #f)
(define (arg3 vm) #f)
(define (global-ref vm var) #f)
(define (global-set! vm var val) #f)
(define (true? val) #f)
(define (false? val) #f)
(define (ensure-argcount found-count expected-count) #f)
(define (ensure-argcount>= found-count expected-count) #f)
(define (add-last seq element) #f)

(define (stepvm vm)
  (fetch-next-instr! vm)
  (inc-pc! vm)
  (let ((opc (current-opcode vm)))
    (cond 
     
     ;; machine control
     ((= opc HALT) (vm-halt-set! vm #t))
     (else (error (string-append "Unknown opcode: "
                                 (object->string instr)))))))

(define (display-vm-status vm)
  (newline)
  (display "Bard VM state:")(newline)
  (display "  pc: ")(display (vm-pc vm))(newline)
  (display "  nargs: ")(display (vm-nargs vm))(newline)
  (display "  instr: ")(display (vm-instr vm))(newline)
  (display "  halt: ")(display (vm-halt vm))(newline)
  (display "  env: ")(display (vm-env vm))(newline)
  (display "  code: ")(display (vm-code vm))(newline)(newline)
  (display "Bard VM finished.")(newline))

(define (runvm vm)
  (let loop ()
    (if (vm-halt vm)
        (begin (display-vm-status vm)
               (stack-top vm))
        (begin (stepvm vm)
               (loop)))))


;;; (define $code (vector (make-instr HALT #f #f)))
;;; (define $env (make-env '()))
;;; (define $fn (make-fn 'testfn $code $env))
;;; (define $vm (make-vm $fn $code 0 $env '() 0 #f #f))
;;; (display-vm-status $vm)
;;; (fetch-next-instr! $vm)
;;; (inc-pc! $vm)
;;; (stepvm $vm)
