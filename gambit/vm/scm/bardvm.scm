;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          bardvm.scm
;;;; Project:       BardVM
;;;; Purpose:       bard vm implmenetation
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

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

(define (vm%reg vm reg)
  (vector-ref (vm:%get-registers vm) reg)
  vm)

(define (vm%reg-set! vm reg val)
  (vector-set! (vm:%get-registers vm) reg val)
  vm)

