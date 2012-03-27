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
(##include "type-macros.scm")

;;; ---------------------------------------------------------------------
;;; primitive types
;;; ---------------------------------------------------------------------

(define-type bard:primitive-type
  id: EE47736A-3F6E-4AEE-899D-09EFA0DEB5E4
  constructor: bard:%make-primitive-type
  (name bard:%type-name)
  (tag bard:%type-tag))

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
(define tags:$structure (%primitive-type-tag (current-input-port)))

;;; ---------------------------------------------------------------------
;;; bard types
;;; ---------------------------------------------------------------------

;;; concrete types

(define $bard-primitive-type-table (make-table test: eqv?))

(bard:define-primitive-type <undefined> tags:$undefined)

(bard:define-primitive-type <null> tags:$null)
(bard:define-primitive-type <character> tags:$character)
(bard:define-primitive-type <boolean> tags:$boolean)
(bard:define-primitive-type <symbol> tags:$symbol)
(bard:define-primitive-type <keyword> tags:$keyword)
(bard:define-primitive-type <flonum> tags:$flonum)
(bard:define-primitive-type <ratio> tags:$ratio)
(bard:define-primitive-type <fixnum> tags:$fixnum)
(bard:define-primitive-type <bignum> tags:$bignum)
(bard:define-primitive-type <closure> tags:$closure)
(bard:define-primitive-type <cons> tags:$pair)
(bard:define-primitive-type <text> tags:$text)
(bard:define-primitive-type <structure> tags:$structure)

;;; structure types

(define-type bard:structure-type
  id: FCD7B5F9-2FA4-49F9-AF7A-22BE656A3633
  constructor: bard:%make-structure-type
  (name bard:%structure-type-name)
  (predicate bard:%structure-type-predicate))

(define $bard-structure-types '())

(define (%def-structure-type name pred)
  (let ((tp (bard:%make-structure-type name pred)))
    (set! $bard-structure-types
          (cons (cons pred tp)
                $bard-structure-types))
    tp))

(define (%obj->structure-type obj)
  (let loop ((entries $bard-structure-types))
    (if (null? entries)
        #f
        (let* ((entry (car entries))
               (more (cdr entries))
               (pred (car entry))
               (type (cdr entry)))
          (if (pred obj)
              type
              (loop more))))))

(bard:define-structure-type <input-stream> input-port?)
(bard:define-structure-type <output-stream> output-port?)

;;; category types

(define-type bard:category
  id: 47065A1E-5CB4-4DD0-A304-312F3B052316
  constructor: bard:%make-category
  (name bard:%category-name))

(define $bard-categories '())

(define (%def-category name)
  (let ((tp (bard:%make-category name)))
    (set! $bard-categories
          (cons (cons name tp)
                $bard-categories))
    tp))

(bard:define-category Anything)

;;; ---------------------------------------------------------------------
;;; type accessors
;;; ---------------------------------------------------------------------

(define (%primitive-type thing)
  (table-ref $bard-primitive-type-table (%primitive-type-tag thing)))

(define (%structure-type thing)
  (%obj->structure-type thing))

(define (%object->bard-type thing)
  (if (##structure? thing)
      (%structure-type thing)
      (%primitive-type thing)))

(define (bard:type? thing)
  (or (bard:primitive-type? thing)
      (bard:structure-type? thing)
      (bard:category? thing)))