;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          types.scm
;;;; Project:       Bard VM
;;;; Purpose:       representation of Bard types
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

;;; ---------------------------------------------------------------------
;;; The representation of types
;;; ---------------------------------------------------------------------
;;; Bard reuses Gambit's primitive types. Most of them are not
;;; exposed in the Bard language.
;;; we arrange type-tag numbers into a convenient contiguous range
;;; to make it easy to index into tables, and to add new types
;;; Gambit's type tags take 4 values:
;;;
;;; 0 - fixnum
;;; 1 - subtyped
;;;     types with an additional 5-bit tag to identify them
;;;     the majority of Gambit types
;;; 2 - special
;;;     a small number of special types, including null, char,
;;;     boolean, and a few others
;;; 3 - pair
;;;
;;; Subtyped values have subtypes in the range 0-31. We assing them
;;; type numbers equal to subtype+4. Thus, vector, whose subtype
;;; is 0, gets type number 4, and the full set of subtypes
;;; get type numbers in the range 4-35.
;;;
;;; there are just a few Gambit special types. they get the range
;;; 36-63, just in case we need more of them in the future.  Bard's
;;; own system types get the range 64-127. User-defined types get type
;;; numbers in the range 128-16777215.
;;; 
;;; Singletons form a special case. Each singleton is a value that
;;; wraps another Bard value, and poses as a type. Singletons are
;;; used to represent values as types for eql specializers in
;;; method dispatch. Singletons are assigned type numbers from 
;;; 16777216 on up.

;;; base types

(define typenum:fixnum 0)
(define typenum:subtyped 1)
(define typenum:special 2)
(define typenum:pair 3)

;;; subtypes

(define typenum:vector        4) ; 0
(define typenum:unused-pair          5) ; 1
(define typenum:ratnum        6) ; 2
(define typenum:cpxnum        7) ; 3
(define typenum:structure     8) ; 4
(define typenum:boxvalues     9) ; 5
(define typenum:meroon       10) ; 6

(define typenum:symbol       12) ; 8
(define typenum:keyword      13) ; 9
(define typenum:frame        14) ; 10
(define typenum:continuation 15) ; 11
(define typenum:promise      16) ; 12
(define typenum:weak         17) ; 13
(define typenum:procedure    18) ; 14
(define typenum:return       19) ; 15

(define typenum:foreign      22) ; 18
(define typenum:string       23) ; 19
(define typenum:s8vector     24) ; 20
(define typenum:u8vector     25) ; 21
(define typenum:s16vector    26) ; 22
(define typenum:u16vector    27) ; 23
(define typenum:s32vector    28) ; 24
(define typenum:u32vector    29) ; 25
(define typenum:f32vector    30) ; 26

(define typenum:s64vector    31) ; 27
(define typenum:u64vector    32) ; 28
(define typenum:f64vector    33) ; 29
(define typenum:flonum       34) ; 30
(define typenum:bignum       35) ; 31

;;; special

(define typenum:null         36)
(define typenum:char         37)
(define typenum:boolean      38)
(define typenum:unbound      39)

;;; Bard system types

(define $min-system-typenumber 64)
(define $max-system-typenumber 127)
(define $min-user-typenumber 128)
(define $max-user-typenumber 16777215)
(define $min-singleton-typenumber 16777216)

(define %basetype  ##type)

(define %subtype  ##subtype)

(define (%typenumber x)
  (let ((t (##type x)))
    (case t
      ((0) 0)
      ((1) (+ 4 (%subtype x)))
      ((2) (cond ((null? x)    typenum:null)
                 ((char? x)    typenum:char)
                 ((boolean? x) typenum:boolean)
                 ((eq? x #!unbound) typenum:unbound)
                 (else (error (str "Unrecognized subtype: " x)))))
      ((3) 3)
      (else (error (str "Unrecognized type: " x))))))

;;; ---------------------------------------------------------------------
;;; type objects
;;; ---------------------------------------------------------------------
;;; type objects are data structures used to represent types as values
;;; in the Bard runtime.

;;; Representations of type objects
;;; -------------------------------

;;; the common supertype of all type objects
(define-type %type
  id: EE47736A-3F6E-4AEE-899D-09EFA0DEB5E4
  extender: %defsubtype
  read-only:
  debug-name
  number)

;;; types directly represented by Gambit built-in types
(%defsubtype %primitive-type constructor: %make-primitive-type)

;;; types represented by Gambit's define-type structures
(%defsubtype %structure-type constructor: %make-structure-type)

;;; Schemas
;;; -------

;;; the common supertype of user-defined table-like and vector-like types
(%defsubtype %schema extender: %define-schema)

;;; the type of table-like schemas
(%define-schema %table-type constructor: %make-table-type)

;;; representations of table instances
(%defsubtype %table constructor: %make-table)

;;; the type of vector-like schemas
(%define-schema %vector-type constructor: %make-vector-type)

;;; representations of vector instances
(%defsubtype %vector constructor: %make-vector)

;;; Protocols and Functions
;;; -----------------------

;;; Bard protocols
(%defsubtype %protocol constructor: %make-protocol)

;;; Bard functions
(%defsubtype %function constructor: %make-function)

;;; Bard methods
(%defsubtype %method constructor: %make-method)

;;; Singletons
;;; ----------

;;; the and representation of singletons--bard values treated as types
(%defsubtype %singleton
  constructor: %make-singleton
  read-only:
  (value %singleton-value))

;;; ---------------------------------------------------------------------
;;; Bard's built-in types
;;; ---------------------------------------------------------------------
;;; utilities for creating type objects
;;; definitions of standard type objects

;;; type tables

(define $typenumber->type-object-vector (make-vector 512 #f))
(define $typenumber->type-name-vector (make-vector 512 #f))
(define $type-name->typenumber-table (make-table test: eq?))
(define $bard-singletons (make-table test: eqv?))

;;; assigning type numbers

(define $next-available-system-typenumber $min-system-typenumber)
(define $next-available-user-typenumber $min-user-typenumber)
(define $next-available-singleton-typenumber $min-singleton-typenumber)

(define $min-singleton-typenumber 16777216)

(define (get-next-available-system-typenumber)
  (let ((num $next-available-system-typenumber))
    (if (> num $max-system-typenumber)
        (error (str "Invalid system typenumber: " num))
        (begin
          (set! $next-available-system-typenumber
                (+ 1 $next-available-system-typenumber))
          num))))

(define (get-next-available-user-typenumber)
  (let ((num $next-available-user-typenumber))
    (if (or (< num $min-user-typenumber)
            (> num $max-user-typenumber))
        (error (str "Invalid user typenumber: " num))
        (begin
          (set! $next-available-user-typenumber
                (+ 1 $next-available-user-typenumber))
          num))))

(define (get-next-available-singleton-typenumber)
  (let ((num $next-available-singleton-typenumber))
    (if (< num $min-singleton-typenumber)
        (error (str "Invalid singleton typenumber: " num))
        (begin
          (set! $next-available-singleton-typenumber
                (+ 1 $next-available-singleton-typenumber))
          num))))

;;; asserting new types

(define (%assert-type! type-object #!key (debug-name #f)(system #f)(tag #f))
  (let ((tag (or tag 
                 (if system
                     (get-next-available-system-typenumber)
                     (get-next-available-user-typenumber)))))
    (if (and (not system)
             (< tag $bard-min-user-type-number ))
        (error (str "Can't create a user type with tag: " tag))
        (begin
          (vector-set! $typenumber->type-object-vector tag type-object)
          (vector-set! $typenumber->type-name-vector tag debug-name)
          (if debug-name
              (table-set! $type-name->typenumber-table debug-name type-object))))))

(define (%define-primitive-type #!key (debug-name #f)(system #f)(tag #f))
  (let ((type-object (%make-primitive-type debug-name tag)))
    (%assert-type! type-object debug-name: debug-name system: system tag: tag)
    type-object))

(define (%define-structure-type #!key (debug-name #f)(system #f)(tag #f))
  (let ((type-object (%make-structure-type debug-name tag)))
    (%assert-type! type-object debug-name: debug-name system: system tag: tag)
    type-object))

(define (%define-protocol #!key (debug-name #f)(system #f)(tag #f))
  (let ((type-object (%make-protocol debug-name tag)))
    (%assert-type! type-object debug-name: debug-name system: system tag: tag)
    type-object))

;;; creating singletons

(define (%singleton val)
  (let* ((oid (object->serial-number val))
         (found (table-ref $bard-singletons oid #f)))
    (or found
        (let ((s (%make-singleton (str "singleton " (object->string val)) (%typenumber val) val)))
          (table-set! $bard-singletons oid s)
          s))))

;;; ---------------------------------------------------------------------
;;; basic types
;;; ---------------------------------------------------------------------

(define <undefined> (%define-primitive-type debug-name: '<undefined> system: #t tag: typenum:unbound))
(define <null> (%define-primitive-type debug-name: '<null> system: #t tag: typenum:null))
(define <character> (%define-primitive-type debug-name: '<character> system: #t tag: typenum:char))
(define <boolean> (%define-primitive-type debug-name: '<boolean>  system: #t tag: typenum:boolean))
(define <symbol> (%define-primitive-type debug-name: '<symbol>  system: #t tag: typenum:symbol))
(define <keyword> (%define-primitive-type debug-name: '<keyword>  system: #t tag: typenum:keyword))
(define <flonum> (%define-primitive-type debug-name: '<flonum>  system: #t tag: typenum:flonum))
(define <ratnum> (%define-primitive-type debug-name: '<ratnum>  system: #t tag: typenum:ratnum))
(define <fixnum> (%define-primitive-type debug-name: '<fixnum>  system: #t tag: typenum:fixnum))
(define <bignum> (%define-primitive-type debug-name: '<bignum>  system: #t tag: typenum:bignum))
(define <primitive-procedure> (%define-primitive-type debug-name: '<primitive-procedure> system: #t tag: typenum:procedure))
(define <string> (%define-primitive-type debug-name: '<string>  system: #t tag: typenum:string))
(define <pair> (%define-primitive-type debug-name: '<pair>  system: #t tag: typenum:pair))
(define <foreign-value> (%define-primitive-type debug-name: '<foreign-value>  system: #t tag: typenum:foreign))
(define <iostream> (%define-structure-type debug-name: '<iostream>  system: #t tag: (get-next-available-system-typenumber)))
(define Anything (%define-protocol debug-name: 'Anything  system: #t tag: (get-next-available-system-typenumber)))
(define Type (%define-protocol debug-name: 'Type  system: #t tag: (get-next-available-system-typenumber)))

;;; type of optional args in method signatures
(define & (%define-protocol debug-name: '&  system: #t tag: (get-next-available-system-typenumber)))

;;; (define <function> )
;;; (define <method> )
;;; (define <protocol> )
;;; (define Schema )
;;; (define <table-schema> )
;;; (define <vector-schema> )
;;; (define <simple-table> )
;;; (define <simple-vector> )

