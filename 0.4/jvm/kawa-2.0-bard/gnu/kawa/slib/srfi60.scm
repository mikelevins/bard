;; SRFI-60 implementation for Kawa (almost everything is already built
;; in, but some of the names differ slightly).
;; Copyright (C) 2014 by Jamison Hope.  The implementations of
;; integer->list, list->integer, and booleans->integer were taken with
;; slight modifications from the reference implementation of SRFI-60,
;; which is copyright (C) 1991, 1993, 2001, 2003, 2005 Aubrey Jaffer
;
;Permission to copy this software, to modify it, to redistribute it,
;to distribute modified versions, and to use it for any purpose is
;granted, subject to the following restrictions and understandings.
;
;1.  Any copy made of this software must include this copyright notice
;in full.
;
;2.  I have made no warranty or representation that the operation of
;this software will be error-free, and I am under no obligation to
;provide any services, by way of maintenance, update, or otherwise.
;
;3.  In conjunction with products arising from the use of this
;material, there shall be no use of my name in any advertising,
;promotional, or sales literature without prior written consent in
;each case.

(module-compile-options warn-unknown-member: #t)

(provide 'srfi-60)

;;; These procedures are already Kawa built-ins and do not need to be
;;; defined here: logand/bitwise-and, logior/bitwise-ior,
;;; logxor/bitwise-xor, lognot/bitwise-not, bitwise-if, logtest,
;;; logcount, integer-length, and ash/arithmetic-shift.

(define-alias arithmetic-shift gnu.kawa.functions.BitwiseOp:ashift)
(define-alias ash gnu.kawa.functions.BitwiseOp:ashift)
(define-alias bitwise-and gnu.kawa.functions.BitwiseOp:and)
(define-alias logand gnu.kawa.functions.BitwiseOp:and)
(define-alias bitwise-ior gnu.kawa.functions.BitwiseOp:ior)
(define-alias logior gnu.kawa.functions.BitwiseOp:ior)
(define-alias bitwise-not gnu.kawa.functions.BitwiseOp:not)
(define-alias lognot gnu.kawa.functions.BitwiseOp:not)
(define-alias bitwise-xor gnu.kawa.functions.BitwiseOp:xor)
(define-alias logxor gnu.kawa.functions.BitwiseOp:xor)
(define-alias integer-length kawa.lib.numbers:bitwise-length)
(define-alias bitwise-if kawa.lib.numbers:bitwise-if)
(define-alias logtest kawa.lib.numbers:logtest)
(define-alias logcount kawa.lib.numbers:logcount)

;;; These procedures alias functionality provided by built-ins with
;;; differing names:

(define bitwise-merge       bitwise-if)
(define any-bits-set?       logtest)
(define bit-count           logcount)
(define log2-binary-factors bitwise-first-bit-set)
(define first-set-bit       bitwise-first-bit-set)
(define bit-field           bitwise-bit-field)
(define reverse-bit-field   bitwise-reverse-bit-field)

;;; These procedures are similar to built-ins but with arguments
;;; reordered:

(define (logbit? index::int n::integer) ::boolean
  (bitwise-bit-set? n index))
(define bit-set? logbit?)

(define (copy-bit-field to::integer from::integer start::int end::int)
  ::integer
  (bitwise-copy-bit-field to start end from))

(define (rotate-bit-field n::integer count::int start::int end::int)
  ::integer
  (bitwise-rotate-bit-field n start end count))

;;; This procedure has a slightly different signature compared to the
;;; built-in bitwise-copy-bit: the first two arguments are swapped and
;;; the last is a boolean instead of an int
(define (copy-bit index::int from::integer bit::boolean)
  ::integer
  (bitwise-copy-bit from index (if bit 1 0)))

;;; These procedures are entirely new, with implementations derived
;;; from the SRFI-60 reference.
(define (integer->list k::integer #!optional (len ::int (integer-length k)))
  ::list
  (do ((idx ::int (- len 1) (- idx 1))
       (k ::integer k (ash k -1))
       (lst ::list '() (cons (odd? k) lst)))
      ((< idx 0) lst)))

(define (list->integer bools::list) ::integer
  (do ((bs bools (cdr bs))
       (acc ::integer 0 (if (car bs) (+ acc acc 1) (+ acc acc))))
      ((null? bs) acc)))

(define (booleans->integer . bools)
  (list->integer bools))
