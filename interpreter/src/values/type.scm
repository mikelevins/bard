;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          types.scm
;;;; Project:       Bard
;;;; Purpose:       reresentation of types in Bard
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(include "~~lib/_gambit#.scm")

;;; ---------------------------------------------------------------------
;;; primitive types
;;; ---------------------------------------------------------------------

(define (%primitive-type-tag obj)
 (let ((t (##type obj)))
   (cond ((fx= t (macro-type-fixnum))
          32)
         ((fx= t (macro-type-special))
          (cond ((null? obj)    33)
                ((char? obj)    34)
                ((boolean? obj) 35)
                (else           36)))
         (else
          (##subtype obj)))))

(define (%find-bignum)
  (let loop ((i 2))
    (if (##bignum? i)
        i
        (loop (* i i)))))

(define tags:$undefined (%primitive-type-tag #!unbound))
(define tags:$null (%primitive-type-tag '()))
(define tags:$boolean (%primitive-type-tag #t))
(define tags:$character (%primitive-type-tag #\c))
(define tags:$fixnum (%primitive-type-tag 1))
(define tags:$bignum (%primitive-type-tag (%find-bignum)))
(define tags:$flonum (%primitive-type-tag 1.2))
(define tags:$ratio (%primitive-type-tag 2/3))
(define tags:$text (%primitive-type-tag "foo"))
(define tags:$pair (%primitive-type-tag '(a . b)))
(define tags:$symbol (%primitive-type-tag 'foo))
(define tags:$keyword (%primitive-type-tag foo:))
(define tags:$vector (%primitive-type-tag (vector 0 1)))
(define tags:$closure (%primitive-type-tag (let ((x '()))(lambda () x))))

;;; ---------------------------------------------------------------------
;;; bard types
;;; ---------------------------------------------------------------------

(define-type bard:type
  id: EE47736A-3F6E-4AEE-899D-09EFA0DEB5E4
  constructor: bard:%make-type
  (name bard:%type-name)
  (tag bard:%type-tag))

;;; concrete types

(define $bard-type-table (make-table test: eqv?))

(define <undefined> (bard:%make-type '<undefined> tags:$undefined))
(table-set! $bard-type-table tags:$undefined <undefined>)

(define <null> (bard:%make-type '<null> tags:$null))
(table-set! $bard-type-table tags:$null <null>)

(define <character> (bard:%make-type '<character> tags:$character))
(table-set! $bard-type-table tags:$character <character>)

(define <boolean>  (bard:%make-type '<boolean> tags:$boolean))
(table-set! $bard-type-table tags:$boolean <boolean>)

(define <symbol> (bard:%make-type '<symbol> tags:$symbol))
(table-set! $bard-type-table tags:$symbol <symbol>)

(define <keyword> (bard:%make-type '<keyword> tags:$keyword))
(table-set! $bard-type-table tags:$keyword <keyword>)

(define <flonum> (bard:%make-type '<flonum> tags:$flonum))
(table-set! $bard-type-table tags:$flonum <flonum>)

(define <ratio> (bard:%make-type '<ratio> tags:$ratio))
(table-set! $bard-type-table tags:$ratio <ratio>)

(define <fixnum> (bard:%make-type '<fixnum> tags:$fixnum))
(table-set! $bard-type-table tags:$fixnum <fixnum>)

(define <bignum> (bard:%make-type '<bignum> tags:$bignum))
(table-set! $bard-type-table tags:$bignum <bignum>)

(define <closure> (bard:%make-type '<closure> tags:$closure))
(table-set! $bard-type-table tags:$closure <closure>)

(define <cons> (bard:%make-type '<cons> tags:$pair))
(table-set! $bard-type-table tags:$pair <cons>)

(define <text> (bard:%make-type '<text> tags:$text))
(table-set! $bard-type-table tags:$text <text>)

;;; ---------------------------------------------------------------------
;;; API
;;; ---------------------------------------------------------------------

(define (bard:representation? thing)
  (and (bard:type? thing)
       (bard:%type-tag thing)))

(define (bard:type thing)
  (or (table-ref $bard-type-table (%primitive-type-tag thing))
      (error "unknown type" thing)))
