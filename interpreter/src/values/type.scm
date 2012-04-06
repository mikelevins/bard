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

(include "~~lib/_gambit#.scm")
(##include "type-macros.scm")

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
(define tags:$ratnum (%primitive-type-tag 2/3))
(define tags:$string (%primitive-type-tag "foo"))
(define tags:$pair (%primitive-type-tag '(a . b)))
(define tags:$symbol (%primitive-type-tag 'foo))
(define tags:$keyword (%primitive-type-tag foo:))
(define tags:$procedure (%primitive-type-tag (lambda () x)))
(define tags:$structure (%primitive-type-tag (current-input-port)))

(define-type %primitive-type
  id: EE47736A-3F6E-4AEE-899D-09EFA0DEB5E4
  constructor: %make-primitive-type
  (name %primitive-type-name)
  (tag %primitive-type-tag))

;;; ---------------------------------------------------------------------
;;; bard types
;;; ---------------------------------------------------------------------

(define $bard-primitive-type-table (make-table test: eqv?))

;;; structure types

(define-type %structure-type
  id: FCD7B5F9-2FA4-49F9-AF7A-22BE656A3633
  constructor: %private-make-structure-type
  (name %structure-type-name)
  (predicate %structure-type-predicate))

(define $bard-structure-types '())

(define (%make-structure-type name pred)
  (let ((tp (%private-make-structure-type name pred)))
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

;;; singletons

(define-type %singleton
  id: F735A1E4-9D1C-4FB2-8E22-BA4FD08B637C
  constructor: %singleton
  (value %singleton-value))

;;; ---------------------------------------------------------------------
;;; type accessors
;;; ---------------------------------------------------------------------

(define (%primitive-type thing)
  (table-ref $bard-primitive-type-table (%primitive-type-tag thing)))

(define (%structure-type thing)
  (%obj->structure-type thing))

(define (%object->bard-type thing)
  (if (%singleton? thing)
      (%object->bard-type (%singleton-value thing))
      (if (##structure? thing)
          (%structure-type thing)
          (%primitive-type thing))))

(define (%type? thing)
  (or (%singleton? thing)
      (%primitive-type? thing)
      (%structure-type? thing)
      (%protocol? thing)))

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

(%define-structure-type <input-stream> input-port?)
(%define-structure-type <output-stream> output-port?)

;;; Bard structure types

(define-type %frame
  id: 87DD4EB3-09F7-41A4-BEED-0B74FF5C92CE
  constructor: %make-frame
  (slots %frame-slots %set-frame-slots!))

(%define-structure-type <frame> %frame?)

(define-type %function
  id: C612A269-DA79-48F2-9FA0-F5F8F329EEBC
  constructor: %private-make-function
  (name %function-name)
  (method-table %function-method-table %set-function-method-table!))

(%define-structure-type <function> %function?)

(define-type %method
  id: 86F8548C-056C-4369-ADF3-1657D7E83649
  constructor: %private-make-method
  (name %method-name)
  (parameters %method-parameters)
  (body %method-body))

(%define-structure-type <method> %method?)

;;; protocol types

(%define-protocol Anything)

;;; ---------------------------------------------------------------------
;;; type taxonomy
;;; ---------------------------------------------------------------------

(define (%subtype? t1 t2)
  (if (equal? t1 t2)
      #t
      (if (%singleton? t2)
          #f
          (if (%singleton? t1)
              (%subtype? (%singleton-value t1) t2)
              (if (equal? t2 Anything)
                  #t
                  #f)))))

