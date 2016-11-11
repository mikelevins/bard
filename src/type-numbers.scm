;;;; ***********************************************************************
;;;;
;;;; Name:          type-numbers.scm
;;;; Project:       Bard
;;;; Purpose:       computing identifying numbers for all bard types
;;;; Author:        mikel evins
;;;; Copyright:     2016 by mikel evins
;;;;
;;;; ***********************************************************************


(declare (extended-bindings))

;;;---------------------------------------------------------------------
;;; ABOUT
;;;---------------------------------------------------------------------
;;; bard's implementation reuses and extends Gambit's system of
;;; type tagging in order to map values to the structure objects
;;; that represent their concrete datatypes

;;; ---------------------------------------------------------------------
;;; general utilities
;;; ---------------------------------------------------------------------

;;; we use this to reliably get a bignum from which to retrieve the
;;; tag
(define (%find-bignum)
  (let loop ((i 2))
    (if (##bignum? i)
        i
        (loop (* i i)))))

;;; ---------------------------------------------------------------------
;;; bard type numbers
;;; ---------------------------------------------------------------------
;;; a bard type number is an integer assigned to a structure to
;;; uniquely identify it. the type number can be efficiently computed
;;; for any bard value. we use type numbers to efficiently identify
;;; and retrieve the structure objects that represent the datatypes of
;;; values.
;;;
;;; a bard type number is a 30-bit integer. its bits are used like so:
;;;
;;; 00 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29
;;;  ^     ^                    |  ^                                                        |
;;;  |     |                    |  |                                                        |
;;;  |     |                    |  ---- bard structure tag ---------------------------------|
;;;  |     ---- Gambit subtype -|
;;;  ---- Gambit type 
;;;
;;; In built-in Gambit types the bard structure tag is always zero.
;;;
;;; bard structure tags identify the data structures used to represent
;;; fundamental bard data representations. User-defined types do not
;;; get unique structure tags. The lower-level data structures used to
;;; represent their types and instances do. for example, defining a
;;; new record type in bard does not allocate a new structure tag; the
;;; new structure type uses the existing structure tag for records,
;;; and instances use the existing structure tag for record instances.
;;; however, if a new version of bard introduces a new kind of
;;; fundamental structure type, a new structure tag is allocated for it, 
;;; and another one for its instances.

;;; ---------------------------------------------------------------------
;;; constructing and deconstructing type numbers
;;; ---------------------------------------------------------------------
;;; a type number is constructed by combining a 10-bit gambit type+subtype
;;; tag with a 20-bit bard structure tag

(define $gambit-type-mask      #b000000000000000000000000000011)
(define $gambit-subtype-mask   #b000000000000000000001111111100)
(define $bard-structure-mask   #b111111111111111111110000000000)

(define (integer->gambit-type-tag n) n)
(define (gambit-type-tag->integer n) n)
(define (integer->gambit-subtype-tag n)(arithmetic-shift n 2))
(define (gambit-subtype-tag->integer n)(arithmetic-shift n -2))
(define (integer->bard-structure-tag n)(arithmetic-shift n 10))
(define (bard-structure-tag->integer n)(arithmetic-shift n -10))

(define $no-structure 0)

(define (type-number gambit-type-tag gambit-subtype-tag bard-structure-tag)
  (+ (integer->gambit-type-tag gambit-type-tag)
     (integer->gambit-subtype-tag gambit-subtype-tag)
     (integer->bard-structure-tag bard-structure-tag)))

(define (type-number->gambit-type-tag n)
  (gambit-type-tag->integer (bitwise-and n $gambit-type-mask)))

(define (type-number->gambit-subtype-tag n)
  (gambit-subtype-tag->integer (bitwise-and n $gambit-subtype-mask)))

(define (type-number->bard-structure-tag n)
  (bard-structure-tag->integer (bitwise-and n $bard-structure-mask)))

;;; ---------------------------------------------------------------------
;;; getting gambit built-in type and subtype tags
;;; ---------------------------------------------------------------------

(define (%gambit-type-tag object)
  (##type object))

;;; gambit's 'special' type is used for atomic values like '() and #t and #f
(define (%gambit-special? object)
  (= 2 (%gambit-type-tag object)))

;;; gambit's 'subtyped' type is used for things like vectors which
;;; have a bunch of specialized subtypes (e.g. u8vector and s16vector, et. al.)
(define (%gambit-subtype-tag object)
  (if (##subtyped? object)
      (##subtype object)
      0))

;;; ---------------------------------------------------------------------
;;; bard structure tags
;;; ---------------------------------------------------------------------
;;; bard structure tags uniquely identify bard-defined structures.
;;; all Gambit-defined datatypes except Gambit's 'special' types
;;; have bard structure type zero. 'special' values are primitive
;;; atomic values that represent special Scheme values like
;;; '() and #t. Gambit gives all of these type tag == 2. So that
;;; bard can distinguish them, we give each of the ones bard
;;; knows about unique bard structure numbers.

(define bard-structure-tag:nothing 1)
(define bard-structure-tag:true 2)
(define bard-structure-tag:false 3)
(define bard-structure-tag:end 4)
(define bard-structure-tag:undefined 5)

(define (%bard-structure-tag thing)
  (cond
   ;; Gambit's 'special' values
   ((eq? thing '()) bard-structure-tag:nothing)
   ((eq? thing #t) bard-structure-tag:true)
   ((eq? thing #f) bard-structure-tag:false)
   ((eq? thing #!eof) bard-structure-tag:end)
   ((eq? thing #!unbound) bard-structure-tag:undefined)
   ;; TODO: add cases here for bard structures and instances as soon
   ;;       as I get them defined
   ;;
   ;; all other values:
   (else 0)))

;;; ---------------------------------------------------------------------
;;; getting bard structure numbers
;;; ---------------------------------------------------------------------

(define (bard-structure-number-of thing)
  (type-number (%gambit-type-tag thing)
               (%gambit-subtype-tag thing)
               (%bard-structure-tag thing)))

