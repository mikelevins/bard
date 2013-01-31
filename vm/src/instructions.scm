;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          instructions.scm
;;;; Project:       Bard
;;;; Purpose:       VM instructions
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

(define HALT  0)
(define CONST 1)
(define JUMP  2)
(define FJUMP 3)
(define LREF  4)
(define LSET  5)
(define GT    6)
(define ADD   7)

(define-macro (pushv! v) `(push! ,v $vals))
(define-macro (popv!) `(pop! $vals))
(define-macro (incpc!) `(set! $pc (+ $pc 1)))
(define-macro (setpc! d) `(set! $pc ,d))
(define-macro (op i) `(vector-ref ,i 0))
(define-macro (arg1 i) `(vector-ref ,i 1))
(define-macro (arg2 i) `(vector-ref ,i 2))
(define-macro (lref i j) `(env-ref $env ,i ,j))
(define-macro (lset! i j v) `(env-set! $env ,i ,j ,v))

(define %halt (lambda (instr)($haltfn)))
(define %const (lambda (instr)(pushv! (arg1 instr))(incpc!)))
(define %jump (lambda (instr)(setpc! (arg1 instr))))
(define %fjump (lambda (instr)(if (popv!)(incpc!)(setpc! (arg1 instr)))))
(define %lref (lambda (instr)(pushv! (lref (arg1 instr)(arg2 instr)))(incpc!)))
(define %lset (lambda (instr)(begin (lset! (arg1 instr)(arg2 instr)(popv!))(incpc!))))
(define %gt (lambda (instr)(let ((a (popv!))(b (popv!)))(pushv! (> a b)))(incpc!)))
(define %add (lambda (instr)(let ((a (popv!))(b (popv!)))(pushv! (+ a b)))(incpc!)))

(define $instructions
  (vector %halt %const %jump %fjump %lref %lset %gt %add))
