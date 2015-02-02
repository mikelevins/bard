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
;;; define
;;; ensure
;;; if
;;; let
;;; loop
;;; macro
;;; quote
;;; set!
;;; with-exit

