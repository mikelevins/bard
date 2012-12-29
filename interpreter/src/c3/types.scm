;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          types.scm
;;;; Project:       Bard
;;;; Purpose:       bard type representations
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

;;; ---------------------------------------------------------------------
;;; primitive gambit types
;;; ---------------------------------------------------------------------

(define (%primitive-tag obj)
 (let ((t (##type obj)))
   (cond ((fx= t 0) ; fixnum tag
          32)
         ((fx= t 2) ; "special" tag
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

(define tags:$undefined (%tag #!unbound))
(define tags:$null (%tag '()))
(define tags:$boolean (%tag #t))
(define tags:$character (%tag #\c))
(define tags:$fixnum (%tag 1))
(define tags:$bignum (%tag (%find-bignum)))
(define tags:$flonum (%tag 1.2))
(define tags:$ratnum (%tag 2/3))
(define tags:$string (%tag "foo"))
(define tags:$pair (%tag '(a . b)))
(define tags:$symbol (%tag 'foo))
(define tags:$keyword (%tag foo:))
(define tags:$procedure (%tag (lambda (x) x)))
(define tags:$structure (%tag (current-input-port)))
(define tags:$vector (%tag (vector)))
(define tags:$box (%tag (box 1)))
(define tags:$foreign-value 18)

;;; the maximum type tage used at this point is 36.
;;; We'll introduce a margin for error and start
;;; subsequent type-tag numbers at 128.

(define *next-type-tag* 128)

(define (%next-available-type-tag)
  (set! *next-type-tag* (+ 1 *next-type-tag*))
  *next-type-tag*)

;;; ---------------------------------------------------------------------
;;; representing bard types
;;; ---------------------------------------------------------------------
;;; primitive-type | standard-type | protocol | class | singleton |
;;; record-schema (user-defined) | vector-schema (user-defined) |
;;; structure-type (gambit type represented by a gambit structure)

(define-type %type
  id: EE47736A-3F6E-4AEE-899D-09EFA0DEB5E4
  extender: defsubtype
  read-only:
  (name %type-name)
  (tag %type-tag))

(defsubtype %primitive-type constructor: %make-primitive-type)
(defsubtype %standard-type constructor: %make-standard-type)
(defsubtype %protocol constructor: %make-protocol)
(defsubtype %class constructor: %make-class)

(defsubtype %structure-type 
  constructor: %make-structure-type
  ;; the prototype is a gambit-defined structure
  ;; instances of this type are instances of the prototype
  (prototype %structure-type-prototype))

(defsubtype %singleton
  constructor: %make-singleton
  read-only:
  (value %singleton-value))

(defsubtype %schema extender: defschema)
(defschema %record-schema constructor: %make-record-schema)
(defschema %vector-schema constructor: %make-vector-schema)

(defsubtype %schema-instance
  extender: def-schema-instance
  (schema %instance-schema))

(def-schema-instance %record-instance
  (slots %instance-slots %set-instance-slots!))

(def-schema-instance %vector-instance
  (elements %instance-elements %set-instance-elements!))

(define (%tag thing)
  (cond
   ((%type? thing) (%type-tag thing))
   ((%schema-instance? thing)(%type-tag (%instance-schema thing)))
   ((##structure? thing) (table-ref $bard-structure-types (##structure-type thing) #f))
   (else (%primitive-tag thing))))

(define (%tag->type tag)(table-ref $bard-types tag #f))

(define (%type thing)
  (cond
   ((%primitive-type? thing) PrimitiveType)
   ((%standard-type? thing) StandardType)
   ((%protocol? thing) Protocol)
   ((%class? thing) Class)
   ((%structure-type? thing) StructureType)
   ((%record-schema? thing) RecordSchema)
   ((%vector-schema? thing) VectorSchema)
   ((%singleton? thing) (%type (%singleton-value thing)))
   ((%type? thing) Type)
   ((%schema-instance? thing)(%tag->type (%tag thing)))
   ((##structure? thing)(%tag->type (%tag thing)))
   (else (%tag->type (%primitive-tag thing)))))

;;; ---------------------------------------------------------------------
;;; interning singletons
;;; ---------------------------------------------------------------------

(define $bard-singletons (make-table test: equal?))

(define (%existing-singleton val)(table-ref $bard-singletons val #f))

;;; there is no reason to expose this as a user function
;;; because singletons as first-class values provide no
;;; useful use cases
;;; instead, the evaluator should arrange to call it for 
;;; method definitions where singleton arguments are specified

(define (%singleton val)
  (let ((found (%existing-singleton val)))
    (or found
        (let ((s (%make-singleton (string-append "singleton " (object->string val))
                                  (%tag val)
                                  val)))
          (table-set! $bard-singletons val s)
          s))))

;;; ---------------------------------------------------------------------
;;; definers for built-in types
;;; ---------------------------------------------------------------------

(define $bard-types (make-table test: eqv?))
(define $bard-structure-types (make-table test: eqv?))

(define (%assert-type! tag tp)
  (table-set! $bard-types tag tp)
  tp)

(define (%define-primitive-type name tag)
  (%assert-type! tag (%make-primitive-type name tag)))

(define (%define-standard-type name #!optional (tag (%next-available-type-tag)))
  (%assert-type! tag (%make-standard-type name tag)))

(define (%define-protocol name #!optional (tag (%next-available-type-tag)))
  (%assert-type! tag (%make-protocol name tag)))

(define (%define-class name #!optional (tag (%next-available-type-tag)))
  (%assert-type! tag (%make-class name tag)))

(define (%define-structure-type name prototype #!optional (tag (%next-available-type-tag)))
  (let ((tp (%make-structure-type name tag prototype)))
    (%assert-type! tag tp)
    (table-set! $bard-structure-types prototype tag)
    tp))

(define (%define-record-schema name #!optional (tag (%next-available-type-tag)))
  (%assert-type! tag (%make-record-schema name tag)))

(define (%define-vector-schema name #!optional (tag (%next-available-type-tag)))
  (%assert-type! tag (%make-vector-schema name tag)))

;;; ---------------------------------------------------------------------
;;; definitions of built-in types
;;; ---------------------------------------------------------------------

;;; Primitive types

(define <undefined> (%define-primitive-type '<undefined> tags:$undefined))
(define <null> (%define-primitive-type '<null> tags:$null))
(define <character> (%define-primitive-type '<character> tags:$character))
(define <boolean> (%define-primitive-type '<boolean> tags:$boolean))
(define <symbol> (%define-primitive-type '<symbol> tags:$symbol))
(define <keyword> (%define-primitive-type '<keyword> tags:$keyword))
(define <flonum> (%define-primitive-type '<flonum> tags:$flonum))
(define <ratnum> (%define-primitive-type '<ratnum> tags:$ratnum))
(define <fixnum> (%define-primitive-type '<fixnum> tags:$fixnum))
(define <bignum> (%define-primitive-type '<bignum> tags:$bignum))
(define <primitive-procedure> (%define-primitive-type '<primitive-procedure> tags:$procedure))
(define <string> (%define-primitive-type '<string> tags:$string))
(define <pair> (%define-primitive-type '<pair> tags:$pair))
(define <foreign-value> (%define-primitive-type '<foreign-value> tags:$foreign-value))

;;; Standard types

(define <iostream> (%define-structure-type '<iostream> (##structure-type (current-input-port))))

;;; Classes

(define & (%define-class '&)) ; type of optional args in method signatures
(define Anything (%define-class 'Anything))
(define Class (%define-class 'Class))
(define Protocol (%define-class 'Protocol))
(define Type (%define-class 'Type))
(define Schema (%define-class 'Schema))
(define RecordSchema (%define-class 'RecordSchema))
(define VectorSchema (%define-class 'VectorSchema))
(define StructureType (%define-class 'StructureType))
(define PrimitiveType (%define-class 'PrimitiveType))
(define StandardType (%define-class 'StandardType))


