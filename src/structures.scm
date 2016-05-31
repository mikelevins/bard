;;;; ***********************************************************************
;;;;
;;;; Name:          structures.scm
;;;; Project:       Bard
;;;; Purpose:       representing bard's structure types
;;;; Author:        mikel evins
;;;; Copyright:     2016 by mikel evins
;;;;
;;;; ***********************************************************************


(declare (extended-bindings))

;;;---------------------------------------------------------------------
;;; ABOUT
;;;---------------------------------------------------------------------
;;; the basic bard datatypes are called 'structures'. They are distinct
;;; from bard 'classes', which are named open collections of structures
;;; defined by protocols.
;;;
;;; This file defines the Scheme data structures that we use to represent
;;; bard structures and classes.
;;;
;;; There is a distinct representation for each individual structure,
;;; but they fall into six kinds of representation:
;;;
;;; 1. named-literals are a small number of unique named literal
;;;    values: true, false, nothing, end, and undefined
;;;
;;; 2. atomic structures represent basic built-in datatypes
;;;    without significant internal structure, such as
;;;    integers, booleans, and characters. Built-in Scheme
;;;    datatypes are exposed through bard as atomic types.
;;;
;;; 3. record structures represent datatypes made up of named slots.
;;;
;;; 4. tuple structures represent datatypes made up of slots
;;;    addressed by index.
;;;
;;; 5. synonyms represent names for other existing datatypes.
;;;
;;; 6. singletons represent specific values presented as types
;;;    for dispatch purposes. 

;;; ---------------------------------------------------------------------
;;; named-literal-structure
;;; ---------------------------------------------------------------------
;;; distinctive, primitive, named, built-in atoms

;;; nothing
;;; ---------------------------------------------------------------------

(define (bard:nothing) '())

(define (bard:nothing? thing)
  (null? thing))

(define (bard:something? thing)
  (not (nothing? thing)))

;;; true and false
;;; ---------------------------------------------------------------------

(define (bard:false) #f)
(define (bard:true) #t)

(define (bard:false? thing)
  (or (eqv? thing (bard:false))
      (eqv? thing (bard:nothing))))

(define (bard:true? thing)
  (not (bard:false? thing)))

;;; end
;;; ---------------------------------------------------------------------

(define (bard:end) #!eof)

(define (bard:end? thing)
  (eqv? thing #!eof))

;;; undefined
;;; ---------------------------------------------------------------------

(define (bard:undefined) #!unbound)

(define (bard:undefined? thing)
  (eqv? thing #!unbound))

(define (bard:defined? thing)
  (not (bard:undefined? thing)))

;;; ---------------------------------------------------------------------
;;; atomic-structure
;;; ---------------------------------------------------------------------
;;; the atomic structures are primitive built-in datatypes such as
;;; characters and integers, either provided by the Gambit scheme
;;; rutnime or implemented as Scheme records that are not accessible
;;; for redefinition in bard. Examples of datatypes that bard
;;; sees as atomic-structure types are pair, fixnum, and character.

;;; the defined atomic structures
;;; for each of these bard needs:
;;; - a type predicate
;;; - a constructor
;;; - a method on structure-of
;;; - accessors
;;; - a method on print
;;; - a reader for literal syntax

;;; Names
;;;
;;; keyword
;;; symbol
;;; uri

;;; Numbers
;;;
;;; small-integer
;;; big-integer
;;; single-float
;;; double-float

;;; s8ratio
;;; u8ratio
;;; s16ratio
;;; u16ratio
;;; s32ratio
;;; u32ratio
;;; s64ratio
;;; u64ratio

;;; Text characters
;;;
;;; character
;;; Sequences
;;;
;;; vector
;;; string
;;; s8vector
;;; u8vector
;;; s16vector
;;; u16vector
;;; s32vector
;;; u32vector
;;; s64vector
;;; u64vector

;;; Maps
;;; alist-map
;;; hashtable
;;; treemap

;;; Pairs
;;; cons

;;; Procedures
;;; method
;;; function

;;; IO streams
;;; character-input-stream
;;; character-output-stream
;;; octet-input-stream
;;; octet-output-stream
;;; object-input-stream
;;; object-output-stream

;;; ---------------------------------------------------------------------
;;; record-structure
;;; ---------------------------------------------------------------------
;;; record structures are representations of sets of named slots.
;;; TODO: define a way to map record names to structure descriptions.
;;;       a simple hashtable is inadequate because we need a way to
;;;       handle what happens when a record structure is redefined
;;;       after instances of the old structure have been created.

;;; record-structure API:
;;;
;;; define record constructs:
;;; - the constructor
;;; - the type predicate
;;; - a method on structure-of
;;; - accesors for each named field
;;; - a method on print

;;; ---------------------------------------------------------------------
;;; tuple-structure
;;; ---------------------------------------------------------------------
;;; tuple structures are representations of sequences of indexed slots
;;; TODO: define a way to map tuple names to structure descriptions.
;;;       a simple hashtable is inadequate because we need a way to
;;;       handle what happens when a tuple structure is redefined
;;;       after instances of the old structure have been created.

;;; tuple-structure API:
;;;
;;; define record constructs:
;;; - the constructor
;;; - the type predicate
;;; - a method on structure-of
;;; - indexed accesors
;;; - a method on print

;;; ---------------------------------------------------------------------
;;; synonym-structure
;;; ---------------------------------------------------------------------
;;; synonyms are representations of user-defined named aliases for
;;; other existing types

;;; ---------------------------------------------------------------------
;;; singleton-structure
;;; ---------------------------------------------------------------------
;;; singletons are representations of individual values viewed as types
;;; for dispatch purposes


