;; This implementation is very loosely based on Richard Kelsey's
;; reference implementation.  http://srfi.schemers.org/srfi-9/srfi-9.html
;; That implementation has the following copyright:
;; Copyright (C) Richard Kelsey (1999). All Rights Reserved.
;; This document and translations of it may be copied and furnished to
;; others, and derivative works that comment on or otherwise explain
;; it or assist in its implementation may be prepared, copied,
;; published and distributed, in whole or in part, without restriction
;; of any kind, provided that the above copyright notice and this
;; paragraph are included on all such copies and derivative
;; works. However, this document itself may not be modified in any
;; way, such as by removing the copyright notice or references to the
;; Scheme Request For Implementation process or editors, except as
;; needed for the purpose of developing SRFIs in which case the
;; procedures for copyrights defined in the SRFI process must be
;; followed, or as required to translate it into languages other than
;; English.

(module-export define-record-type)

(require <kawa.lib.prim_syntax>)
(require <kawa.lib.std_syntax>)
(import (only (kawa standard begin) begin))
(import (rename (only (kawa standard define_class) define_class)
                (define_class define-class)))
(import (rename (only (kawa standard Scheme) instanceOf) (instanceOf instance?)))
(import (rename (only (gnu kawa reflect SlotGet) slotRef) (slotRef slot-ref)))
(import (rename (only (gnu kawa reflect SlotSet) set-field!) (set-field! slot-set!)))
(import (only (gnu kawa reflect Invoke) make))
(import (rename (only (kawa lang Quote) plainQuote) (plainQuote quote)))

(define-syntax define-record-type
  (syntax-rules ()
    ((define-record-type type
       (constructor constructor-tag ...)
       predicate
       (field-tag accessor . more) ...)
     (begin
       (define-class type () interface: #f
	 (field-tag) ...)
       (define (predicate obj) :: <boolean>
	 (instance? obj type))
       (define (constructor constructor-tag ...) :: type
	 (let ((tmp :: type (make type)))
	   (begin (slot-set! tmp 'constructor-tag constructor-tag) ...)
	   tmp))
       (%define-record-field type field-tag accessor . more)
       ...))))

; An auxilliary macro for define field accessors and modifiers.
; This is needed only because modifiers are optional.

(define-syntax %define-record-field
  (syntax-rules ()
    ((%define-record-field type field-tag accessor)
     (define (accessor (obj :: type))
       (slot-ref obj 'field-tag)))
    ((%define-record-field type field-tag accessor modifier)
     (begin
       (define (accessor (obj :: type))
	 (slot-ref obj 'field-tag))
       (define (modifier (obj :: type) value) :: <void>
	 (slot-set! obj 'field-tag value))))))
