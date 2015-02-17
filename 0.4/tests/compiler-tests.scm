;;;; ***********************************************************************
;;;;
;;;; Name:          compiler-tests.scm
;;;; Project:       Bard
;;;; Purpose:       tests of the Bard compiler
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(declare (extended-bindings))

;;; constants
(define $env0 '())
(kernel:eval (bard:compile '())  $env0) ; => ()
(kernel:eval (bard:compile 2/3)  $env0) ; => 2/3
(kernel:eval (bard:compile "Foobar")  $env0) ; =>"Foobar"

;;; symbols
(define $env1 '((x . 1)))
(bard:compile 'x '()) ; => x
(kernel:eval (bard:compile 'x '()) $env1) ; => 1
(kernel:eval (bard:compile 'y '()) $env1) ; error: Unbound variable y
(kernel:eval '(DEF y 101) $env1)
(kernel:eval (bard:compile 'y '()) $env1) ; => 101

;;; lambdas
(kernel:eval (bard:compile '(^ (x) x) '()) $env0) ; => new lambda
(kernel:eval (bard:compile '((^ x x) 1 2 3) '()) $env0) ; => 1 2 3

;;; begin
(kernel:eval (bard:compile 'Barney '()) $env0) ; error: unbound variable Barney
(kernel:eval (bard:compile '(begin (DEF Barney "Barney") Barney) '()) $env0) ; => "Barney"

;;; cond
(kernel:eval (bard:compile '(cond ((< 3 1) 'nonsense)((> 3 1) 'YEP)(#t 'default)) '()) $env0) ; => YEP

;;; define
(kernel:eval (bard:compile '(define x 1001) '()) $env0)
(kernel:eval (bard:compile 'x '()) $env0)
(kernel:eval (bard:compile '(define (add x y)(+ x y)) '()) $env0)
(kernel:eval (bard:compile 'add '()) $env0)
(kernel:eval (bard:compile '(add 2 3) '()) $env0)

;;; ensure
(kernel:eval (bard:compile '(ensure (DEF q 2) after: q) '()) $env0) ;=> error: Unbound variable q
(kernel:eval (bard:compile 'q '()) $env0) ; => 2

;;; if
(kernel:eval (bard:compile '(if (> 1 3) 'yep 'nope) '()) $env0) ; => nope

;;; let
(kernel:eval (bard:compile '(let ((x 2)(y (+ x 1))) (* x y)) '()) $env0) ; => 6

;;; loop
;;; macro
;;; quote
(kernel:eval (bard:compile '(quote Frobby) '()) $env0) ; => Frobby

;;; set!
(kernel:eval '(DEF W "double-you") '())
(kernel:eval (bard:compile '(set! W 101) '()) $env0)
(kernel:eval (bard:compile 'W '()) $env0) ; => 101

;;; with-exit
(kernel:eval '(DEF i 0) '())
(kernel:eval (bard:compile '(with-exit (return)
                                       (REPEAT (if (> i 10)
                                                   (return i)
                                                   (set! i (+ i 1)))))
                           '()) $env0) ; => 11




