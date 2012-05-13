;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          type.scm
;;;; Project:       Bard
;;;; Purpose:       representation of Bard types
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(##include "type-macros.scm")

;;; ---------------------------------------------------------------------
;;; primitive types
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
(define tags:$procedure (%type-tag (lambda () x)))
(define tags:$structure (%type-tag (current-input-port)))

(define-type %primitive-type
  id: EE47736A-3F6E-4AEE-899D-09EFA0DEB5E4
  constructor: %make-primitive-type
  (name %primitive-type-name)
  (tag %primitive-type-tag))

(define $bard-primitive-type-table (make-table test: eqv?))

;;; ---------------------------------------------------------------------
;;; bard types
;;; ---------------------------------------------------------------------

;;; structure types

(define-type %structure-type
  id: FCD7B5F9-2FA4-49F9-AF7A-22BE656A3633
  constructor: %make-structure-type
  (name %structure-type-name)
  (gambit-type %structure-type-gambit-type)
  (predicate %structure-type-predicate))

(define $bard-structure-type-table (make-table test: eqv?))

;;; protocol types

(define-type %protocol
  id: 47065A1E-5CB4-4DD0-A304-312F3B052316
  constructor: %private-make-protocol
  (name %protocol-name))

(define $bard-protocols '())

(define (%make-protocol name)
  (let ((tp (%private-make-protocol name)))
    (set! $bard-protocols
          (cons (cons name tp)
                $bard-protocols))
    tp))

(%define-structure-type <protocol> (##structure-type (%make-protocol 'ignored)) %protocol?)

;;; singletons

(define-type %singleton
  id: F735A1E4-9D1C-4FB2-8E22-BA4FD08B637C
  constructor: %private-make-singleton
  (value %singleton-value))

(%define-structure-type <singleton> (##structure-type (%private-make-singleton 'ignored)) %singleton?)

(define $singleton-table (make-table test: equal?))

(define (%singleton x)
  (let ((already? (table-ref $singleton-table x #f)))
    (or already?
        (let ((s (%private-make-singleton x)))
          (table-set! $singleton-table x s)
          s))))

;;; ---------------------------------------------------------------------
;;; define the base bard types
;;; ---------------------------------------------------------------------

;;; primitive types

(%define-primitive-type <undefined> tags:$undefined)
(%define-primitive-type <null> tags:$null)
(%define-primitive-type <character> tags:$character)
(%define-primitive-type <boolean> tags:$boolean)
(%define-primitive-type <symbol> tags:$symbol)
(%define-primitive-type <keyword> tags:$keyword)
(%define-primitive-type <flonum> tags:$flonum)
(%define-primitive-type <ratnum> tags:$ratnum)
(%define-primitive-type <fixnum> tags:$fixnum)
(%define-primitive-type <bignum> tags:$bignum)
(%define-primitive-type <primitive-procedure> tags:$procedure)
(%define-primitive-type <cons> tags:$pair)
(%define-primitive-type <string> tags:$string)

;;; gambit structure types

(%define-structure-type <input-stream> (##structure-type (current-input-port)) input-port?)
(%define-structure-type <output-stream> (##structure-type (current-output-port)) output-port?)

;;; Bard structure types

;;; ---------------------------------------------------------------------
;;; type accessors
;;; ---------------------------------------------------------------------

(define (%primitive-type thing)
  (table-ref $bard-primitive-type-table (%type-tag thing)))

(define (%structure-type thing)
  (table-ref $bard-structure-type-table (##structure-type thing)))

(define (%type? thing)
  (or (%singleton? thing)
      (%primitive-type? thing)
      (%structure-type? thing)
      (%protocol? thing)))

(define-type %type-type
  id: 7EAC4075-6187-426F-A8DE-4DFC15400F62
  constructor: %make-type-type
  (name %type-type-name))

(define <type> (%make-type-type '<type>))

(define (%object->bard-type thing)
  (cond
   ((eq? thing <type>) <type>)
   ((%type? thing) <type>)
   ((##structure? thing) (%structure-type thing))
   (else (%primitive-type thing))))

;;; ---------------------------------------------------------------------
;;; type taxonomy
;;; ---------------------------------------------------------------------

(define (%subtype? t1 t2)
  (if (equal? t1 t2)
      #t
      (if (%singleton? t2)
          #f
          (if (%singleton? t1)
              (%subtype? (%object->bard-type (%singleton-value t1)) t2)
              (if (equal? t2 Anything)
                  #t
                  #f)))))

