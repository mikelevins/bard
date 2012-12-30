;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          primitive-types.scm
;;;; Project:       Bard
;;;; Purpose:       tools for working with built-in gambit types
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
;;; bard's schema table
;;; ---------------------------------------------------------------------

(define $bard-schemas (make-table test: eqv?))

;;; some host-native types are represented by native structures
;;; we keep track of those in these tables
(define $structure-tags (make-table test: eqv?))
(define $structure-names (make-table test: eqv?))
(define $named-structures (make-table test: eqv?))

(define $next-bard-schema-tag 128)
(define (%next-available-schema-tag)
  (set! $next-bard-schema-tag (+ 1 $next-bard-schema-tag))
  $next-bard-schema-tag)

(define (%assert-schema! s #!optional (tag (%next-available-schema-tag)))
  (table-set! $bard-schemas tag s)
  s)

(define (%assert-structure! name struct #!optional (tag (%next-available-schema-tag)))
  (table-set! $bard-schemas tag struct)
  (table-set! $structure-tags struct tag)
  (table-set! $structure-names struct name)
  (table-set! $named-structures name struct)
  struct)

(define (%structure-tag s)(table-ref $structure-tags s #f))
(define (%structure-name s)(table-ref $structure-names s #f))
(define (%name->structure nm)(table-ref $named-structures nm #f))

;;; ---------------------------------------------------------------------
;;; type operations
;;; ---------------------------------------------------------------------

(define (%object->type-tag thing)
  (cond
   ((%schema? thing) (%schema-tag thing))
   ((%record-instance? thing)(%schema-tag (%instance-schema thing)))
   ((##structure? thing) (table-ref $structure-tags (##structure-type thing) #f))
   (else (%tag thing))))

(define (%object->bard-type thing)
  (cond
   ((%schema? thing) Schema)
   ((%record-instance? thing)(let ((tag (%object->type-tag thing)))
                               (if tag
                                   (table-ref $bard-schemas tag #f)
                                   (error (string-append "Can't get the type of " (object->string thing))))))
   ((##structure? thing) (let ((tag (%object->type-tag thing)))
                           (if tag
                               (table-ref $bard-schemas tag #f)
                               (error (string-append "Can't get the type of " (object->string thing))))))
   (else (table-ref $bard-schemas (%tag thing)))))

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
  (if (eq? tp Anything)
      (%true)
      (if (%singleton? tp)
          (eq? (%singleton val) tp)
          (%subtype? (%object->bard-type val) tp))))

(define (%keyed-collection? op)
  (or
   (%null? op)
   (string? op)
   (%list? op)
   (%table? op)))


