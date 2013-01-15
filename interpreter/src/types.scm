;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          types.scm
;;;; Project:       Bard
;;;; Purpose:       tools for working with built-in gambit type tags
;;;;                and bard-specific type tags
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

;;; =====================================================================
;;; prelude
;;; =====================================================================

;;; ---------------------------------------------------------------------
;;; general utilities
;;; ---------------------------------------------------------------------

(define (%find-bignum)
  (let loop ((i 2))
    (if (##bignum? i)
        i
        (loop (* i i)))))

;;; ---------------------------------------------------------------------
;;; built-in schema types
;;; ---------------------------------------------------------------------

(define-type schema extender: define-schema name tag)
(define-type schema-instance extender: define-instance (schema instance-schema))

(define-schema primitive-schema)
(define-schema structure-schema prototype)
(define-schema base-schema)
(define-schema record-schema slots)
(define-instance record-instance slots)
(define-schema tuple-schema slot-count slot-type)
(define-instance tuple-instance slots)
(define-schema union-schema)
(define-instance union-instance variants)

(define-schema foreign-schema type-name)

;;; representation of instances of Bard types

(define-instance alist-table-instance constructor: make-alist-table-instance slots)
(define-instance class-instance constructor: make-class-instance (name class-name))

(define-instance function-instance
  constructor: make-function-instance
  name proc signatures thunk-method method-tree)

(define-instance generator-instance 
  constructor: make-generator-instance
  proc results)

(define-instance interpreted-method-instance
  constructor: make-interpreted-method-instance
  name proc formals restarg required-count environment body)

(define-instance primitive-instance
  constructor: make-primitive-instance
  name proc required-count restarg)

(define-instance protocol-instance 
  constructor: make-protocol-instance
  name
  functions)

(define-instance singleton-instance constructor: make-singleton-instance value)

;;; ---------------------------------------------------------------------
;;; gambit type tags
;;; ---------------------------------------------------------------------

(define tags:$gambit-fixnum 0)
(define tags:$gambit-subtyped 1)
(define tags:$gambit-special 2)
(define tags:$gambit-pair 3) ; BUG: not necessarily correct on all architectures

(define tags:$gambit-subtype-structure 4)

(define (%gambit-type object)(##type object))
(define (%gambit-special? object)(= 2 (##type object)))

(define (%gambit-subtype object)
  (if (##subtyped? object)
      (##subtype object)
      0))

;;; ---------------------------------------------------------------------
;;; gambit "special" objects
;;; ---------------------------------------------------------------------
;;; "special" objects have gambit tag 2
;;; they are immediates representing values like '(), #!eof, and characters
;;; we give them bard tags to distinguish them because gambit doesn't
;;; use tags for that purpose, and we want a uniform type id

(define $max-bard-special-number 255)
(define $unercognized-bard-special-number $max-bard-special-number)

(define (%bard-special-number object)
  (cond
   ((null? object) 0)
   ((boolean? object) 1)
   ((char? object) 2)
   ((eqv? #!void object) 3)
   ((eqv? #!unbound object) 4)
   ((eqv? #!eof object) 5)
   (else $max-bard-special-number)))

;;; =====================================================================
;;; obtaining tags
;;; =====================================================================

;;; a type tag is a 30-bit integer used to store the type and subtype
;;; bits of a gambit value uniformly for indexing purposes. In the
;;; bard integer, the two least-significant bits are the gambit type;
;;; the rest is the gambit subtype.

(define $type-mask      #b000000000000000000000000000011)
(define $subtype-mask   #b000000000000000000001111111100)

(define (integer->subtype n)(arithmetic-shift n 2))
(define (subtype->integer n)(arithmetic-shift n -2))

(define (tag->gambit-type n)(bitwise-and n $type-mask))
(define (tag->gambit-subtype n)(subtype->integer (bitwise-and n $subtype-mask)))

;;; bard type numbers
;;; ---------------------------------------------------------------------
;;; a bard type number is a number assigned to a schema to distinguish
;;; it from other. since gambit type and subtype together occupy only
;;; 7 bits, the tag scheme permits us to store bard type numbers in
;;; tags.  reserve bits 0-9 for gambit type and subtype, and bits
;;; 10-29 for bard type numbers.

(define $next-bard-type-number (+ 1 $max-bard-special-number))
(define (%next-bard-type-number)
  (let ((num $next-bard-type-number))
    (set! $next-bard-type-number (+ 1 $next-bard-type-number))
    num))

(define $bard-type-mask #b111111111111111111110000000000)

(define (integer->bard-type n)(arithmetic-shift n 10))
(define (bard-type->integer n)(arithmetic-shift n -10))

(define (%make-tag type-tag subtype-tag #!optional (bard-tag 0))
  (+ type-tag
     (integer->subtype subtype-tag)
     (integer->bard-type bard-tag)))

(define (tag->bard-type n)(bard-type->integer (bitwise-and n $bard-type-mask)))

(define (%tag val)
  (if (schema-instance? val)
      (schema-tag (instance-schema val))
      (if (##structure? val)
          (let* ((struct (##structure-type val))
                 (schema (%structure->schema struct)))
            (if schema
                (schema-tag schema)
                #f))
          (if (%gambit-special? val)
              (%make-tag (%gambit-type val) 0 (%bard-special-number val))
              (%make-tag (%gambit-type val) (%gambit-subtype val))))))

;;; ---------------------------------------------------------------------
;;; tags for well-known gambit types
;;; ---------------------------------------------------------------------

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
(define tags:$vector (%tag (vector)))
(define tags:$box (%tag (box 1)))

;;; =====================================================================
;;; the schema registry
;;; =====================================================================
;;; the data structures and functions here enable us to recover a schema
;;; given one of its instances.

;;; handling primitive schemas
;;; ---------------------------------------------------------------------

(define +tag->schema-registry+ (make-table test: eqv?))

;;; we recover the tag from the value itself, using gambit's
;;; tag operations (##type and ##subtype), then look up the
;;; schema in the registry.

(define (%register-primitive-schema! sc tag)
  (table-set! +tag->schema-registry+ tag sc))

(define (%tag->schema tag)
  (table-ref +tag->schema-registry+ tag #f))

;;; handling structure schemas
;;; ---------------------------------------------------------------------
;;; recover the structure used to create the instance using
;;; ##structure-type, then recover the schema from the structure
;;; registry

(define +structure->schema-registry+ (make-table test: eqv?))

(define (%register-structure-schema! structure-type schema)
  (table-set! +structure->schema-registry+ structure-type schema))

(define (%structure->schema struct)
  (table-ref +structure->schema-registry+ struct #f))

;;; handling foreign schemas
;;; ---------------------------------------------------------------------
;;; registry for foreign schemas
(define +foreign-name->schema-registry+ (make-table test: eqv?))

;;; base schemas
;;; ---------------------------------------------------------------------
;;; we recover the schema from the instance, using instance-schema; no
;;; registry is necessary

