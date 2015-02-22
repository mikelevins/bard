(module-static #f)
(module-export my-factorial list-length-2 deldup call-to-first
	       date mod1-v5 <simpleAux> 
               counter counter-macro
	       test1-import0 mod0-v1 mod0-v2 mod0-v3 mod0-f1 mod0-m1
	       namespace-syntax-test make-array make-array-fun mA)
(require <module0>)

(define (get1-mod0-v1) mod0-v1)
(define (set1-mod0-v1 x) (set! mod0-v1 x))

(define mod1-v5 15)

(define (test1-import0)
  (let ((gv1 (get1-mod0-v1)))
    (set1-mod0-v1 (+ 14 gv1))
    (list gv1 mod0-v1 mod0-v2 (mod0-f1))))

(define (my-factorial n)
  (if (<= n 1)
      1
      (* n (my-factorial (- n 1)))))

(define (list-length-1 (x :: <list>)) :: <double>
   (length x))
(define (list-length-2 x) :: <int>
  (include-relative "included-1.scm")
  (inexact->exact z2))

(require 'list-lib)

(define-syntax deldup
  (syntax-rules ()
   ((deldup list)
    (delete-duplicates list))))

(define (call-to-first x)
  (first x))

(define-namespace date "class:java.util.Date")

(define-syntax namespace-syntax-test
  (syntax-rules ()
    ((namespace-syntax-test)
     (date:parse "6 Sep 2003 UTC"))))

;; Test for Savannah bug #11508 submitted by Thomas Kirk <tk@research.att.com>:
(define-syntax make-array
  (syntax-rules ()
    ((make-array len type)
     ((primitive-array-new type) len))
    ((make-array len)
     ((primitive-array-new <java.lang.Object>) len))))
(define (make-array-fun n)
  (make-array n))

;; From Savannah bug#11822, contributed by Dean Ferreyra.
;; (Other parts of this testcase are in module3.scm and obj-test.scm.)
(define-simple-class <simpleAux> ()
  (x 5)
  ((init) (set! x 100)))
(define-syntax mA
  (syntax-rules ()
    ((_ type forms ...)
     (define-simple-class type ()
       forms ...))))

;; Andre van Tonder <andre@het.brown.edu> example in posting 2011-04-19.
(define counter (let ((n 0)) (lambda () (set! n (+ n 1)) n)))
(define-syntax counter-macro (syntax-rules () ((_) (counter))))
