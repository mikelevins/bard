;;;; ***********************************************************************
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
;;; built-in bard-structure types
;;; ---------------------------------------------------------------------

(define-type bard-structure extender: define-bard-structure name tag)
(define-type bard-structure-instance extender: define-instance (bard-structure instance-bard-structure))

(define-bard-structure primitive-structure)
(define-bard-structure structure-bard-structure prototype)
(define-bard-structure base-bard-structure)

(define-bard-structure foreign-bard-structure type-name)

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

(define-instance url-instance 
  constructor: make-url-instance
  string)

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
;;; a bard type number is a number assigned to a bard-structure to distinguish
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
  (if (bard-structure-instance? val)
      (bard-structure-tag (instance-bard-structure val))
      (if (##structure? val)
          (let* ((struct (##structure-type val))
                 (bard-structure (%structure->bard-structure struct)))
            (if bard-structure
                (bard-structure-tag bard-structure)
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
;;; the bard-structure registry
;;; =====================================================================
;;; the data structures and functions here enable us to recover a bard-structure
;;; given one of its instances.

;;; handling primitive bard-structures
;;; ---------------------------------------------------------------------

(define +tag->bard-structure-registry+ (make-table test: eqv?))

;;; we recover the tag from the value itself, using gambit's
;;; tag operations (##type and ##subtype), then look up the
;;; bard-structure in the registry.

(define (%register-primitive-structure! sc tag)
  (table-set! +tag->bard-structure-registry+ tag sc))

(define (%tag->bard-structure tag)
  (table-ref +tag->bard-structure-registry+ tag #f))

;;; handling structure bard-structures
;;; ---------------------------------------------------------------------
;;; recover the structure used to create the instance using
;;; ##structure-type, then recover the bard-structure from the structure
;;; registry

(define +structure->bard-structure-registry+ (make-table test: eqv?))

(define (%register-structure-bard-structure! structure-type bard-structure)
  (table-set! +structure->bard-structure-registry+ structure-type bard-structure))

(define (%structure->bard-structure struct)
  (table-ref +structure->bard-structure-registry+ struct #f))

;;; handling foreign bard-structures
;;; ---------------------------------------------------------------------
;;; registry for foreign bard-structures
(define +foreign-name->bard-structure-registry+ (make-table test: eqv?))

;;; base bard-structures
;;; ---------------------------------------------------------------------
;;; we recover the bard-structure from the instance, using instance-bard-structure; no
;;; registry is necessary

