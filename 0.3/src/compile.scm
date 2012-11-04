;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          compile.scm
;;;; Project:       Bard
;;;; Purpose:       bard compiler
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(define (%syntax-type expr)
  (car expr))

(define (%const-syntax->value expr)
  (let ((stype (%syntax-type expr)))
    (case stype
    ((syntax:character) (cadr expr))
    ((syntax:empty-app) '())
    ((syntax:empty-list) '())
    ((syntax:empty-string) '())
    ((syntax:empty-table) '())
    ((syntax:false) #f)
    ((syntax:keyword) (cadr expr))
    ((syntax:nothing) '())
    ((syntax:number) (cadr expr))
    ((syntax:self-evaluating) (cadr expr))
    ((syntax:table) (cadr expr))
    ((syntax:text) (cadr expr))
    ((syntax:true) #t)
    ((syntax:undefined) #!unbound)
    (else (error (str "Invalid constant syntax: " expr))))))

(define (%compile-app expr env)
  `(ir:APP ,expr))

(define (%compile-character expr env)
  `(ir:CHARACTER ,(cadr expr)))

(define (%compile-number expr env)
  (let ((num (cadr expr)))
    (cond
     ((##bignum? num) `(ir:BIGNUM num))
     ((##fixnum? num) `(ir:FIXNUM num))
     ((##flonum? num) `(ir:FLONUM num))
     ((##ratnum? num) `(ir:RATNUM num))
     (else (error (str "Invalid number syntax: " expr))))))

(define (%compile-list expr env)
  (cons 'ir:LIST (map (lambda (x)(%compile x env)) (cdr expr))))

(define (%compile-table expr env)
  (cons 'ir:TABLE (map (lambda (x)(%compile x env)) (cdr expr))))

(define (%compile-self-evaluating expr env)
  `(ir:CONST ,(%const-syntax->value expr)))

(define (%compile-method-lambda-list expr env)
  (cdr expr))

(define (%compile-method expr env)
  `(ir:METHOD ,expr))

(define (%compile-series-expression expr env)
  `(ir:SERIES ,expr))

(define (%compile-variable-reference expr env)
  `(ir:VAR ,(cadr expr)))

(define (%compile-nothing expr env)
  `(ir:NOTHING))

(define (%compile-false expr env)
  `(ir:FALSE))

(define (%compile-true expr env)
  `(ir:TRUE))

(define (%compile-undefined expr env)
  `(ir:UNDEFINED))

(define (%get-compiler-for stype)
  (case stype
    ((syntax:app) %compile-app)
    ((syntax:character) %compile-character)
    ((syntax:empty-app) %compile-nothing)
    ((syntax:empty-list) %compile-nothing)
    ((syntax:empty-string) %compile-nothing)
    ((syntax:empty-table) %compile-nothing)
    ((syntax:false) %compile-false)
    ((syntax:keyword) %compile-self-evaluating)
    ((syntax:lambda-list) %compile-method-lambda-list)
    ((syntax:list) %compile-list)
    ((syntax:method) %compile-method)
    ((syntax:nothing) %compile-nothing)
    ((syntax:number) %compile-number)
    ((syntax:self-evaluating) %compile-self-evaluating)
    ((syntax:series) %compile-series-expression)
    ((syntax:symbol) %compile-variable-reference)
    ((syntax:table) %compile-table)
    ((syntax:text) %compile-self-evaluating)
    ((syntax:true) %compile-true)
    ((syntax:undefined) %compile-undefined)
    (else #f)))

(define (%compile expr env)
  (let ((_compile (%get-compiler-for (%syntax-type expr)))) 
    (if _compile
        (_compile expr env)
        (error (str "Syntax error: " expr)))))

;;; (%compile (bard:read-from-string "#\\C") '())
;;; (%compile (bard:read-from-string "#\\space") '())
;;; (%compile (bard:read-from-string "#\\u0041") '())
;;; (%compile (bard:read-from-string "false") '())
;;; (%compile (bard:read-from-string "true") '())
;;; (%compile (bard:read-from-string "Foo:") '())
;;; (%compile (bard:read-from-string "nothing") '())
;;; (%compile (bard:read-from-string "()") '())
;;; (%compile (bard:read-from-string "[]") '())
;;; (%compile (bard:read-from-string "{}") '())
;;; (%compile (bard:read-from-string "\"\"") '())
;;; (%compile (bard:read-from-string "[0 1 2 3]") '())
;;; (%compile (bard:read-from-string "(^ (x)(* x x))") '())
;;; (%compile (bard:read-from-string "99999999999999999999999999999999999999999999999999999999999") '())
;;; (%compile (bard:read-from-string "999") '())
;;; (%compile (bard:read-from-string "9.99") '())
;;; (%compile (bard:read-from-string "1.3e+12") '())
;;; (%compile (bard:read-from-string "2/3") '())
;;; (%compile (bard:read-from-string "foo") '())
;;; (%compile (bard:read-from-string "Bar") '())
;;; (%compile (bard:read-from-string "|Foo Bar|") '())
;;; (%compile (bard:read-from-string "{}") '())
;;; (%compile (bard:read-from-string "{a: 1 b: 2}") '())
;;; (%compile (bard:read-from-string "(~ x in: [1 2])") '())
;;; (%compile (bard:read-from-string "(~ x in: NATURAL where: (odd? x))") '())
;;; (%compile (bard:read-from-string "(~ with: [[x 0] [y 1]] yield: [x y] then: [y (+ y 1)])") '())
;;; (%compile (bard:read-from-string "\"Foo bar baz\"") '())
;;; (%compile (bard:read-from-string "undefined") '())
