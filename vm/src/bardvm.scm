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
;;; the vm data structure
;;; ---------------------------------------------------------------------

(define-structure vm globals fn code pc env stack nargs instr halt)

;;; ---------------------------------------------------------------------
;;; vm support functions
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

(define (global-ref vm var)
  (let* ((globals (vm-globals vm))
         (entry (assq var globals)))
    (if entry
        (cdr entry)
        (error "Undefined variable: " var))))

(define (global-set! vm var val)
  (let* ((globals (vm-globals vm))
         (entry (assq var globals)))
    (if entry
        (set-cdr! entry val)
        (vm-globals-set! vm
                         (cons (cons var val)
                               (vm-globals vm))))
    val))

(define (arg1 vm)(instr-arg1 (vm-instr vm)))
(define (arg2 vm)(instr-arg2 (vm-instr vm)))

;;; TODO: make true? and false? work with all the relevant bard datatypes
(define (true? val)(if val #t #f))
(define (false? val)(if val #f #t))

(define (display-vm-status vm)
  (newline)
  (display "Bard VM state:")(newline)
  (display "  pc: ")(display (vm-pc vm))(newline)
  (display "  nargs: ")(display (vm-nargs vm))(newline)
  (display "  instr: ")(display (vm-instr vm))(newline)
  (display "  stack: ")(display (vm-stack vm))(newline)
  (display "  halt: ")(display (vm-halt vm))(newline)
  (display "  globals: ")(display (vm-globals vm))(newline)
  (display "  env: ")(display (vm-env vm))(newline)
  (display "  code: ")(display (vm-code vm))(newline)(newline)
  (newline))

;;; ---------------------------------------------------------------------
;;; running the vm
;;; ---------------------------------------------------------------------

;;; execute a single instriction
(define (stepvm vm)
  (fetch-next-instr! vm)
  (inc-pc! vm)
  (let ((opc (current-opcode vm)))
    (cond 
     ;; stack and variables
     ((= opc LVAR) (stack-push! vm (env-ref vm (arg1 vm))))
     ((= opc LSET) (env-set! vm (arg1 vm) (stack-top vm)))
     ((= opc GVAR) (stack-push! vm (global-ref vm (arg1 vm))))
     ((= opc GSET) (global-set! vm (arg1 vm) (stack-top vm)))
     ((= opc POP) (stack-pop! vm))
     ((= opc CONST) (stack-push! vm (arg1 vm)))

     ;; branching
     ((= opc JUMP) (vm-pc-set! vm (arg1 vm)))
     ((= opc FJUMP) (when (false? (stack-pop! vm)) (vm-pc-set! vm (arg1 vm))))
     ((= opc TJUMP) (when (true? (stack-pop! vm)) (vm-pc-set! vm (arg1 vm))))

     ;; machine control
     ((= opc HALT) (vm-halt-set! vm #t))

     ;; unrecognized opcode
     (else (error (string-append "Unknown opcode: " (object->string (vm-instr vm))))))))

;;; run the vm
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

;;; $code0
;;; just halt the vm
;;;
;;; (define $env (make-env '()))
;;; (define $fn (make-fn 'testfn $code0 $env))
;;; (define $vm (make-vm '() $fn $code0 0 $env '() 0 #f #f))

;;; (display-vm-status $vm)
;;; (stepvm $vm)
;;; (time (runvm $vm))

(define $code1 (vector (make-instr CONST 5 #f)
                       (make-instr LSET 'x #f)
                       (make-instr POP #f #f)
                       (make-instr LVAR 'x #f)
                       (make-instr HALT #f #f)))

;;; $code1
;;; set and reference a lexical variable
;;;
;;; (define $env (make-env '()))
;;; (define $fn (make-fn 'testfn $code1 $env))
;;; (define $vm (make-vm '() $fn $code1 0 $env '() 0 #f #f))

;;; (display-vm-status $vm)
;;; (stepvm $vm)
;;; (time (runvm $vm))

(define $code2 (vector (make-instr CONST 5 #f)
                       (make-instr GSET 'x #f)
                       (make-instr POP #f #f)
                       (make-instr GVAR 'x #f)
                       (make-instr HALT #f #f)))

;;; $code2
;;; set and reference a global variable
;;;
;;; (define $env (make-env '()))
;;; (define $fn (make-fn 'testfn $code2 $env))
;;; (define $vm (make-vm '() $fn $code2 0 $env '() 0 #f #f))

;;; (display-vm-status $vm)
;;; (stepvm $vm)
;;; (time (runvm $vm))



