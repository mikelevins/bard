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

(define (%type-tag obj)
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

(define tags:$undefined (%type-tag #!unbound))
(define tags:$null (%type-tag '()))
(define tags:$boolean (%type-tag #t))
(define tags:$character (%type-tag #\c))
(define tags:$fixnum (%type-tag 1))
(define tags:$bignum (%type-tag (%find-bignum)))
(define tags:$flonum (%type-tag 1.2))
(define tags:$ratnum (%type-tag 2/3))
(define tags:$string (%type-tag "foo"))
(define tags:$pair (%type-tag '(a . b)))
(define tags:$symbol (%type-tag 'foo))
(define tags:$keyword (%type-tag foo:))
(define tags:$procedure (%type-tag (lambda (x) x)))
(define tags:$structure (%type-tag (current-input-port)))
(define tags:$vector (%type-tag (vector)))
(define tags:$box (%type-tag (box 1)))
(define tags:$foreign-value 18)

;;; ---------------------------------------------------------------------
;;; Bard type objects
;;; ---------------------------------------------------------------------

(define-type %built-in-type
  id: EE47736A-3F6E-4AEE-899D-09EFA0DEB5E4
  constructor: %make-built-in-type
  (name %built-in-type-name)
  (tag %built-in-type-tag))

(define $bard-built-in-type-table (make-table test: eqv?))
(define (%def-built-in-type name tag)
  (let ((tp (%make-built-in-type name tag)))
    (table-set! $bard-built-in-type-table tag tp)
    tp))

(define (%built-in-type thing)
  (table-ref $bard-built-in-type-table (%type-tag thing)))


(define-type %standard-type
  id: FCD7B5F9-2FA4-49F9-AF7A-22BE656A3633
  constructor: %make-standard-type
  (name %standard-type-name)
  (representation %standard-type-representation) ; the gambit structure prototype
  (predicate %standard-type-predicate))

(define $bard-standard-type-table (make-table test: eqv?))
(define (%def-standard-type name structure-type predicate)
  (let ((tp (%make-standard-type name structure-type predicate)))
    (table-set! $bard-standard-type-table structure-type tp)
    tp))

(define (%standard-type thing)
  (table-ref $bard-standard-type-table (##structure-type thing)))

(define-type %singleton
  id: F735A1E4-9D1C-4FB2-8E22-BA4FD08B637C
  constructor: %private-make-singleton
  (value %singleton-value))

(define $singleton-table (make-table test: equal?))

(define (%singleton x)
  (let ((already? (table-ref $singleton-table x #f)))
    (or already?
        (let ((s (%private-make-singleton x)))
          (table-set! $singleton-table x s)
          s))))

(define-type %type-type
  id: 7EAC4075-6187-426F-A8DE-4DFC15400F62
  constructor: %make-type-type
  (name %type-type-name))

(define-type %anything-type
  id: ED16E187-096C-4FF5-9697-98A18E5FFA74
  constructor: %make-anything-type
  (name %anything-type-name))

(define <undefined> (%def-built-in-type '<undefined> tags:$undefined))
(define <null> (%def-built-in-type '<null> tags:$null))
(define <character> (%def-built-in-type '<character> tags:$character))
(define <boolean> (%def-built-in-type '<boolean> tags:$boolean))
(define <symbol> (%def-built-in-type '<symbol> tags:$symbol))
(define <keyword> (%def-built-in-type '<keyword> tags:$keyword))
(define <flonum> (%def-built-in-type '<flonum> tags:$flonum))
(define <ratnum> (%def-built-in-type '<ratnum> tags:$ratnum))
(define <fixnum> (%def-built-in-type '<fixnum> tags:$fixnum))
(define <bignum> (%def-built-in-type '<bignum> tags:$bignum))
(define <primitive-procedure> (%def-built-in-type '<primitive-procedure> tags:$procedure))
(define <string> (%def-built-in-type '<string> tags:$string))
(define <foreign-value> (%def-built-in-type '<foreign-value> tags:$foreign-value))

(define <iostream> (%def-standard-type '<iostream> (##structure-type (current-input-port))  
                                       (lambda (x)(or (input-port? x)(output-port? x)))))

(define Anything (%make-anything-type 'Anything))
(define <type> (%make-type-type '<type>))

;;; ---------------------------------------------------------------------
;;; type operations
;;; ---------------------------------------------------------------------

(define (%type? thing)
  (or (%type-type? thing)
      (%singleton? thing)
      (%built-in-type? thing)
      (%standard-type? thing)))

(define (%object->bard-type thing)
  (cond
   ((%type? thing) <type>)
   ((##structure? thing) (%standard-type thing))
   (else (%built-in-type thing))))

;;; ---------------------------------------------------------------------
;;; type taxonomy
;;; ---------------------------------------------------------------------

(define (%subtype? t1 t2)
  (if (eq? t1 t2)
      #t
      (if (%singleton? t2)
          #f
          (if (%singleton? t1)
              (%subtype? (%object->bard-type (%singleton-value t1)) t2)
              (if (eq? t2 Anything)
                  #t
                  #f)))))
