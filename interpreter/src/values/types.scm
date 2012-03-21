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

(define $prim-tag-undefined (%primitive-type-tag #!unbound))
(define $prim-tag-null (%primitive-type-tag '()))
(define $prim-tag-boolean (%primitive-type-tag #t))
(define $prim-tag-character (%primitive-type-tag #\c))
(define $prim-tag-fixnum (%primitive-type-tag 1))
(define $prim-tag-bignum (%primitive-type-tag (%find-bignum)))
(define $prim-tag-flonum (%primitive-type-tag 1.2))
(define $prim-tag-ratio (%primitive-type-tag 2/3))
(define $prim-tag-text (%primitive-type-tag "foo"))
(define $prim-tag-pair (%primitive-type-tag '(a . b)))
(define $prim-tag-symbol (%primitive-type-tag 'foo))
(define $prim-tag-keyword (%primitive-type-tag foo:))
(define $prim-tag-vector (%primitive-type-tag (vector 0 1)))

(define-type %reference-structure-type
  id: 618D59C2-8AAC-42B7-ACF4-D7A71E55C5CF
  constructor: %make-reference-structure-type)

(define $prim-tag-structure (%primitive-type-tag (%make-reference-structure-type)))

(define $prim-tag-closure (%primitive-type-tag (let ((x '()))(lambda () x))))

;;; ---------------------------------------------------------------------
;;; bard types
;;; ---------------------------------------------------------------------

(define-type bard:type
  id: EE47736A-3F6E-4AEE-899D-09EFA0DEB5E4
  constructor: bard:%make-type
  (name bard:%type-name)
  (tag bard:%type-tag))

(define Undefined (bard:%make-type 'Undefined $prim-tag-undefined))
(define Null  (bard:%make-type 'Null $prim-tag-null))
(define Boolean  (bard:%make-type 'Boolean $prim-tag-boolean))
(define Character (bard:%make-type 'Character $prim-tag-character))
(define Fixnum (bard:%make-type 'Fixnum $prim-tag-fixnum))
(define Bignum (bard:%make-type 'Bignum $prim-tag-bignum))
(define Float (bard:%make-type 'Float $prim-tag-flonum))
(define Ratio (bard:%make-type 'Ratio $prim-tag-ratio))
(define Text (bard:%make-type 'Text $prim-tag-text))
(define Pair (bard:%make-type 'Pair $prim-tag-pair))
(define Symbol (bard:%make-type 'Symbol $prim-tag-symbol))
(define Keyword (bard:%make-type 'Keyword $prim-tag-keyword))
(define Vector (bard:%make-type 'Vector $prim-tag-vector))
(define Structure (bard:%make-type 'Structure $prim-tag-structure))
(define Closure (bard:%make-type 'Closure $prim-tag-closure))

(define (bard:type? thing)
  (bard:type? thing))