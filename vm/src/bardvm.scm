;;;; ***********************************************************************
;;;;
;;;; Name:          bardvm.scm
;;;; Project:       Bard VM
;;;; Purpose:       the Bard VM main program
;;;; Author:        mikel evins
;;;; Copyright:     2019 by mikel evins
;;;;
;;;; ***********************************************************************

;;; ---------------------------------------------------------------------
;;; the vm structure
;;; ---------------------------------------------------------------------

(define-structure vm fn code pc env stack nargs instr halt)

;;; ---------------------------------------------------------------------
;;; support functions
;;; ---------------------------------------------------------------------

(define (fetch-next-instr! vm)
  (vm-instr-set! vm
                 (vector-ref (vm-code vm)
                             (vm-pc vm))))

(define (inc-pc! vm)(vm-pc-set! vm (+ 1 (vm-pc vm))))

(define (set-pc! vm newpc) (vm-pc-set! vm newpc))

(define (current-opcode vm) (instr-opcode (vm-instr vm)))

(define (stack-push! vm val)
  (vm-stack-set! vm (cons val (vm-stack vm))))

(define (stack-top vm) (car (vm-stack vm)))

(define (stack-pop! vm)
  (let ((val (car (vm-stack vm))))
    (vm-stack-set! vm (cdr (vm-stack vm)))
    val))

(define (stack-npop! vm count) #f)
(define (env-pop! vm) #f)
(define (env-push! vm frame) #f)

(define (env-ref vm var)
  (let* ((env (vm-env vm))
         (entry (assq var (env-vars env))))
    (if entry
        (cdr entry)
        (error "Undefined variable: " var))))

(define (env-set! vm var val)
  (let* ((env (vm-env vm))
         (entry (assq var (env-vars env))))
    (if entry
        (set-cdr! entry val)
        (env-vars-set! (vm-env vm)
                       (cons (cons var val)
                             (vm-env vm))))
    val))

(define (make-env-frame vals) #f)
(define (arg1 vm)(instr-arg1 (vm-instr vm)))
(define (arg2 vm)(instr-arg2 (vm-instr vm)))
(define (global-ref vm var) #f)
(define (global-set! vm var val) #f)
(define (true? val) #f)
(define (false? val) #f)
(define (ensure-argcount found-count expected-count) #f)
(define (ensure-argcount>= found-count expected-count) #f)
(define (add-last seq element) #f)

;;; ---------------------------------------------------------------------
;;; running the vm
;;; ---------------------------------------------------------------------

(define (stepvm vm)
  (fetch-next-instr! vm)
  (inc-pc! vm)
  (let ((opc (current-opcode vm)))
    (cond 
     ;; stack and variables
     ((= opc LVAR) (stack-push! vm (env-ref vm (arg1 vm))))
     ((= opc LSET) (env-set! vm (arg1 vm) (stack-top vm)))
     ((= opc CONST) (stack-push! vm (arg1 vm)))
     ((= opc POP) (stack-pop! vm))

     ;; machine control
     ((= opc HALT) (vm-halt-set! vm #t))

     ;; unrecognized opcode
     (else (error (string-append "Unknown opcode: " (object->string (vm-instr vm))))))))

(define (display-vm-status vm)
  (newline)
  (display "Bard VM state:")(newline)
  (display "  pc: ")(display (vm-pc vm))(newline)
  (display "  nargs: ")(display (vm-nargs vm))(newline)
  (display "  instr: ")(display (vm-instr vm))(newline)
  (display "  stack: ")(display (vm-stack vm))(newline)
  (display "  halt: ")(display (vm-halt vm))(newline)
  (display "  env: ")(display (vm-env vm))(newline)
  (display "  code: ")(display (vm-code vm))(newline)(newline)
  (newline))

(define (runvm vm)
  (let loop ()
    (if (vm-halt vm)
        (begin (display-vm-status vm)
               (display "Bard VM finished.")
               (stack-top vm))
        (begin (stepvm vm)
               (loop)))))

;;; ---------------------------------------------------------------------
;;; test code
;;; ---------------------------------------------------------------------

(define $code0 (vector (make-instr HALT #f #f)))
(define $code1 (vector (make-instr CONST 5 #f)
                       (make-instr LSET 'x #f)
                       (make-instr POP #f #f)
                       (make-instr LVAR 'x #f)
                       (make-instr HALT #f #f)))

;;; $code0
;;; (define $env (make-env '()))
;;; (define $fn (make-fn 'testfn $code0 $env))
;;; (define $vm (make-vm $fn $code0 0 $env '() 0 #f #f))

;;; $code1
;;; (define $env (make-env '()))
;;; (define $fn (make-fn 'testfn $code1 $env))
;;; (define $vm (make-vm $fn $code1 0 $env '() 0 #f #f))


;;; (display-vm-status $vm)
;;; (stepvm $vm)
;;; (time (runvm $vm))


