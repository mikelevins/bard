;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          types.scm
;;;; Project:       Bard
;;;; Purpose:       representation of Bard types
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

;;; ---------------------------------------------------------------------
;;; primitive type utilities
;;; ---------------------------------------------------------------------

(define (%tag obj)
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

;;; ---------------------------------------------------------------------
;;; type definitions
;;; ---------------------------------------------------------------------
;;; primitive | structure | protocol | singleton | user-defined

(define-type %type
  id: EE47736A-3F6E-4AEE-899D-09EFA0DEB5E4
  extender: %defsubtype
  read-only:
  (name %type-name)
  (tag %type-tag))

(%defsubtype %primitive-type constructor: %make-primitive-type)

(%defsubtype %standard-type constructor: %make-standard-type (prototype %type-prototype))

(%defsubtype %protocol constructor: %make-protocol)

(%defsubtype %singleton
  constructor: %make-singleton
  read-only:
  (value %singleton-value))

;;; ---------------------------------------------------------------------
;;; bard's type table
;;; ---------------------------------------------------------------------

;;; types

(define $bard-types (make-table test: eqv?))
(define $bard-structure-tags (make-table test: eqv?))
(define $bard-max-type-tag 0)

(define (%next-available-type-tag)(+ 1 $bard-max-type-tag))

(define (%assert-type! tag tp)
  (table-set! $bard-types tag tp)
  (set! $bard-max-type-tag (max tag $bard-max-type-tag))
  tp)

(define (%define-primitive-type name tag)
  (%assert-type! tag (%make-primitive-type name tag)))

(define (%define-standard-type name prototype #!optional (tag (%next-available-type-tag)))
  (let ((tp (%make-standard-type name tag prototype)))
    (%assert-type! tag tp)
    (table-set! $bard-structure-tags prototype tag)
    tp))

(define (%define-protocol name #!optional (tag (%next-available-type-tag)))
  (%assert-type! tag (%make-protocol name tag)))

;;; singletons

(define $bard-singletons (make-table test: eqv?))

(define (%singleton val)
  (let ((found (table-ref $bard-singletons (object->serial-number val) #f)))
    (or found
        (let ((s (%make-singleton (string-append "singleton " (object->string val)) (%tag val) val)))
          (table-set! $bard-singletons (object->serial-number val) s)
          s))))

;;; ---------------------------------------------------------------------
;;; basic types
;;; ---------------------------------------------------------------------

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
(define <foreign-value> (%define-primitive-type '<foreign-value> tags:$foreign-value))

(define <iostream> (%define-standard-type '<iostream> (##structure-type (current-input-port))))
(define Anything (%define-protocol 'Anything))
(define Type (%define-protocol 'Type))
(define & (%define-protocol '&)) ; type of optional args in method signatures

;;; ---------------------------------------------------------------------
;;; type operations
;;; ---------------------------------------------------------------------

(define (%object->type-tag thing)
  (cond
   ((%type? thing) (%type-tag thing))
   ((##structure? thing) (table-ref $bard-structure-tags (##structure-type thing) #f))
   (else (%tag thing))))

(define (%object->bard-type thing)
  (cond
   ((%type? thing) Type)
   ((##structure? thing) (let ((tag (%object->type-tag thing)))
                           (if tag
                               (table-ref $bard-types tag #f)
                               (error (string-append "Can't get the type of " (object->string thing))))))
   (else (table-ref $bard-types (%tag thing)))))

;;; ---------------------------------------------------------------------
;;; type taxonomy
;;; ---------------------------------------------------------------------

(define (%subtype? t1 t2)
  (if (eq? t1 t2)
      #t
      (if (eq? t2 Anything)
          #t
          #f)))

(define (%instance-of? val tp)
  (if (%singleton? tp)
      (eq? (%singleton val) tp)
      (%subtype? (%object->bard-type val) tp)))

(define (%keyed-collection? op)
  (or
   (%null? op)
   (string? op)
   (%list? op)
   (%frame? op)))


