;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          types-base.scm
;;;; Project:       Bard
;;;; Purpose:       base Bard protocols
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

;;; =====================================================================
;;; base schemas
;;; =====================================================================
;;; ----------------------------------------------------------------------
;;; <protocol>
;;; ----------------------------------------------------------------------

(define tags:$bard-protocol (%next-bard-type-number))
(define <protocol> (make-base-schema '<protocol> tags:$bard-protocol))

;;; constructor

(define (%make-protocol name)(make-protocol-instance <protocol> name (make-table test: eq?)))

;;; accessors

(define (%protocol-ref p fn-name)
  (table-ref (protocol-instance-functions p) fn-name #f))

(define (%protocol-add! p fn-name fn)
  (table-set! (protocol-instance-functions p) fn-name fn)
  p)

(define (%protocol-remove! p fn-name)
  (table-set! (protocol-instance-functions p) fn-name)
  p)

;;; definitions of protocols
;;; ----------------------------------------------------------------------
;;; convention: protocol names are present participles

(define Applying       (%make-protocol 'Applying))       ; applying function-like values
(define Calculating    (%make-protocol 'Calculating))    ; performing arithmetic and other calculating tasks
(define Comparing      (%make-protocol 'Comparing))      ; comparing values for equality
(define Constructing   (%make-protocol 'Constructing))   ; constructing values
(define Converting     (%make-protocol 'Converting))     ; producing values of one type based on inputs of another
(define Listing        (%make-protocol 'Listing))        ; arranging values in lists
(define Mapping        (%make-protocol 'Mapping))        ; arranging values in tables
(define Ordering       (%make-protocol 'Ordering))       ; arranging values by magnitude
(define Pairing        (%make-protocol 'Pairing))        ; arranging values in pairs
(define Reading        (%make-protocol 'Reading))        ; getting values from input streams
(define System         (%make-protocol 'System))         ; system tools
(define TextProcessing (%make-protocol 'TextProcessing)) ; processing text
(define Typing         (%make-protocol 'Typing))         ; discriminating values by type
(define Writing        (%make-protocol 'Writing))        ; putting values into output streams

