;;;; ***********************************************************************
;;;;
;;;; Name:          kernel-tests.scm
;;;; Project:       Bard
;;;; Purpose:       tests of the kernel evaluator
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(declare (extended-bindings))

(define $env0 '())
(define $env1 '((x . 1)))

;;; kernel:eval symbol
;;; ---------------------------------------------------------------------

(kernel:eval 'x $env0) ; => error: Unbound variable x
(kernel:eval 'x $env1) ; => 1

;;; kernel:eval constants
;;; ---------------------------------------------------------------------

(kernel:eval '() $env0) ; => ()
(kernel:eval 1 $env0) ; => 1
(kernel:eval "Foo" $env0) ; => "Foo"
(kernel:eval (vector 1 2 3) $env0) ; => #(1 2 3)

;;; kernel:eval list
;;; ---------------------------------------------------------------------

;;; ASSIGN
(define $env2 '((x . 2)))
(kernel:eval '(ASSIGN x 202) $env2)
(kernel:eval 'x $env2) ; => 202

;;; BEGIN
(kernel:eval '(BEGIN) '()) ; => nothing
(kernel:eval '(BEGIN 1) '()) ; => 1
(kernel:eval '(BEGIN (+ 2 3)(< 2 3)(= 2 3)) '()) ; => #f

;;; COND
(kernel:eval '(COND ((= 2 3) (QUOTE naw))((> 2 3) (QUOTE nope))((< 2 3) (QUOTE yay))) '()) ; => yay

;;; DEF
(kernel:eval 'frob $env0) ; => error: Unbound variable x
(kernel:eval '(DEF frob 1001) $env0)
(kernel:eval 'frob $env0) ; => 1001

;;; ENSURE

(kernel:eval '(ENSURE (DEF ensured? #t) after: unbound-nonsense) '()) ; => error: Unbounde variable unbound-nonsense
(kernel:eval 'ensured? $env0) ; => #t

;;; FN

(kernel:eval '(FN (x) x) $env0) ; => a new kernel-lambda
(kernel:eval '((FN x x) 1 2 3) $env0) ; => (1 2 3)

;;; IF
(define $env3 '((x . 2)(y . 3)))
(kernel:eval '(IF (< y x) (QUOTE YES)(QUOTE NO)) $env3) ; => NO
(kernel:eval '(IF (< x y) (QUOTE YES)(QUOTE NO)) $env3) ; => YES

;;; LOOP

(kernel:eval '(LOOP ((i 0))(IF (< i 10)(AGAIN (+ i 1)) i)) $env0) ; => 10

;;; with-exit
(kernel:eval '(WITH-EXIT (return) (return x)) $env1) ; => 1


