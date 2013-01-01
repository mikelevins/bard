;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          tags.scm
;;;; Project:       Bard
;;;; Purpose:       tools for working with built-in gambit type tags
;;;;                and bard-specific type tags
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

;;; ---------------------------------------------------------------------
;;; general utilities
;;; ---------------------------------------------------------------------

(define (%find-bignum)
  (let loop ((i 2))
    (if (##bignum? i)
        i
        (loop (* i i)))))

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
;;; bard type tags
;;; ---------------------------------------------------------------------

;;; a bard type tag is a 30-bit integer laid out like this:
;;; ttssssssssbbbbbbbbbbbbbbbbbbbb
;;; where t = gambit tag bits
;;;       s = gambit subtag bits
;;;       b = bard tag bits

(define $type-tag-mask      #b000000000000000000000000000011)
(define $subtype-tag-mask   #b000000000000000000001111111100)
(define $bard-type-tag-mask #b111111111111111111110000000000)

(define (integer->subtype-tag n)(arithmetic-shift n 2))
(define (subtype-tag->integer n)(arithmetic-shift n -2))

(define (integer->bard-type-tag n)(arithmetic-shift n 10))
(define (bard-type-tag->integer n)(arithmetic-shift n -10))

(define (%make-tag type-tag subtype-tag bard-tag)
  (+ type-tag
     (integer->subtype-tag subtype-tag)
     (integer->bard-type-tag bard-tag)))

(define (%make-structure-tag bard-structure-tag)
  (let* ((gambit-type tags:$gambit-subtyped)
         (gambit-subtype tags:$gambit-subtype-structure))
    (%make-tag gambit-type gambit-subtype bard-structure-tag)))

(define (bard-type-tag->gambit-type n)(bitwise-and n $type-tag-mask))
(define (bard-type-tag->gambit-subtype n)(subtype-tag->integer (bitwise-and n $subtype-tag-mask)))
(define (bard-type-tag->bard-type n)(bard-type-tag->integer (bitwise-and n $bard-type-tag-mask)))

;;; ---------------------------------------------------------------------
;;; gambit "special" objects
;;; ---------------------------------------------------------------------
;;; "special" objects have gambit tag 2
;;; they are immediates representing values like '(), #!eof, and characters
;;; we give them bard tags to distinguish them because gambit doesn't
;;; use tags for that purpose, and we want a uniform type id

(define $max-bard-special-tag 255)
(define $unercognized-bard-special-tag $max-bard-special-tag)

(define (%bard-special-type object)
  (cond
   ((null? object) 0)
   ((boolean? object) 1)
   ((char? object) 2)
   ((eqv? #!void object) 3)
   ((eqv? #!unbound object) 4)
   ((eqv? #!eof object) 5)
   (else $max-bard-special-tag)))

;;; ---------------------------------------------------------------------
;;; gambit structures
;;; ---------------------------------------------------------------------
;;; structures are instances of types defined using gambit's define-type
;;; macro. some built-in gambit types and all bard types that are not
;;; represented by built-in gambit types are represented as these.
;;; we assign each distinct structure built into bard its own
;;; bard type tag (but not user-defined structures, which we must
;;; distinguish a different way; if we gave user structures their own
;;; type tags then we'd have to maintain a registry of user type tags)

(define $min-bard-structure-tag (+ 1 $max-bard-special-tag))
(define $max-bard-structure-tag #b11111111111111111111)

(define *next-bard-structure-tag* $min-bard-structure-tag)
(define (next-bard-structure-tag)
  (let ((tag *next-bard-structure-tag*))
    (set! *next-bard-structure-tag* (+ 1 *next-bard-structure-tag*))
    tag))

(define $bard-structure-type->tag-table (make-table test: eqv?))
(define $tag->bard-structure-type-table (make-table test: eqv?))

(define (%register-structure-type! bard-tag structure-object)
  (table-set! $bard-structure-type->tag-table structure-object bard-tag)
  (table-set! $tag->bard-structure-type-table bard-tag structure-object)
  structure-object)

(define (bard-structure-type->tag s)(table-ref $bard-structure-type->tag-table s #f))
(define (tag->bard-structure-type tag)(table-ref $tag->bard-structure-type-table tag #f))

(define (%bard-structure-type object)
  (bard-structure-type->tag (##structure-type object)))

;;; ---------------------------------------------------------------------
;;; the bard tag accessor
;;; ---------------------------------------------------------------------

(define (%tag object)
  (if (##subtyped? object)
      ;; it's a subtyped object
      (if (##structure? object)
          ;; it's a structure
          (%bard-structure-type object) ; structures get bard tags
          ;; it's not a structure; we don't give these bard structure types
          (%make-tag (##type object) (##subtype object) 0))
      ;; it's not subtyped
      (if (%gambit-special? object)
          ;; it's a gambit "special" object; these get bard type tags
          (%make-tag (##type object) 0 (%bard-special-type object))
          ;; it's not "special" and not subtyped; the bard tag is just the gambit type tag
          (%make-tag (##type object) 0 0))))

;;; ---------------------------------------------------------------------
;;; well-known types
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

