;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          bardvm.scm
;;;; Project:       BardVM
;;;; Purpose:       bard vm implmenetation
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

;;; ---------------------------------------------------------------------
;;; the VM
;;; ---------------------------------------------------------------------

(define-type bardvm
  id: 6336A58B-0E6A-4EF1-BFAD-0DB264DFEE42
  constructor: %make-bardvm
  ;; registers
  (registers vm:%get-registers vm:%set-registers!))

(define reg:FUNCTION 0)
(define reg:CODE 1)
(define reg:PC 2)
(define reg:ENV 3)
(define reg:STACK 4)
(define reg:STACK-LIMIT 5)
(define reg:STACK-DEPTH 6)
(define reg:PRIMS 7)
(define reg:ARGCOUNT 8)
(define reg:INSTR 9)
(define reg:HALT 10)
(define vm:$register-count (+ 1 reg:HALT))

(define (vm:make #!key 
                 (function #f)(code #f)(pc 0)
                 (env '())(stack '())(stack-limit 1024)
                 (stack-depth 0)(prims #f)(argcount 0)(instruction #f)(halt #f))
  (let ((regvector (vector function code pc env stack
                           stack-limit stack-depth
                           prims argcount instruction halt)))
    (%make-bardvm regvector)))

(define (vm:%reg vm reg)
  (vector-ref (vm:%get-registers vm) reg)
  vm)

(define (vm:%reg-set! vm reg val)
  (vector-set! (vm:%get-registers vm) reg val)
  vm)

(define (vm:error msg)
  (error msg))

;;; ---------------------------------------------------------------------
;;; PC operations
;;; ---------------------------------------------------------------------

(define (vm:incpc vm)
  (vm:%reg-set! vm reg:PC (+ 1 (vm:%reg vm reg:PC))))

;;; ---------------------------------------------------------------------
;;; stack operations
;;; ---------------------------------------------------------------------

(define (vm:%ensure-stack-capacity vm n)
  (if (not (< (+ n (vm:%reg vm reg:STACK-DEPTH))
              (vm:%reg vm reg:STACK-LIMIT)))
      (vm:error "stack full")))

(define (vm:%check-stack-empty vm)
  (if (< (vm:%reg vm reg:STACK-DEPTH) 1)
      (vm:error "stack empty")))

(define (vm:push vm x)
  (vm:%ensure-stack-capacity vm 1)
  (vm:%reg-set! vm reg:STACK-DEPTH (+ 1 (vm:%reg vm reg:STACK-DEPTH)))
  (vm:%reg-set! vm reg:STACK (cons x (vm:%reg vm reg:STACK))))

(define (vm:pop vm)
  (vm:%check-stack-empty vm)
  (vm:%reg-set! vm reg:STACK-DEPTH (- (vm:%reg vm reg:STACK-DEPTH) 1))
  (let ((val (car (vm:%reg vm reg:STACK))))
    (vm:%reg-set! vm reg:STACK (cdr (vm:%reg vm reg:STACK)))
    val))

;;; ---------------------------------------------------------------------
;;; instructions
;;; ---------------------------------------------------------------------

(define (vm:%make-instruction opcode . args)
  (vm:%check-argcount opcode args)
  (apply vector (cons opcode args)))

(define (instruction:%opcode instr)
  (vector-ref instr 0))

(define (instruction:%arg instr index)
  (vector-ref instr index))

;;; ---------------------------------------------------------------------
;;; instruction execution
;;; ---------------------------------------------------------------------

(define (vm:%current-instruction vm)
  (vector-ref (vm:%reg vm reg:CODE)
              (vm:%reg vm reg:PC)))

;;; argcount: 0
(define (vm:noop vm)
  (vm:incpc vm)
  vm)

;;; argcount: 1
(define (vm:const vm)
  (vm:push vm (instruction:%arg (vm:%current-instruction vm) 1))
  vm)
