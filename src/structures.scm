;;;; ***********************************************************************
;;;;
;;;; Name:          structures.scm
;;;; Project:       Bard
;;;; Purpose:       representing bard's structure types
;;;; Author:        mikel evins
;;;; Copyright:     2016 by mikel evins
;;;;
;;;; ***********************************************************************

(declare (standard-bindings))

;;; ---------------------------------------------------------------------
;;; atomic-structure
;;; ---------------------------------------------------------------------
;;; the atomic structures are primitive built-in datatypes such as
;;; characters and integers, either provided by the Gambit scheme
;;; rutnime or implemented as Scheme records that are not accessible
;;; for redefinition in bard. Examples of datatypes that bard
;;; sees as atomic-structure types are pair, fixnum, and character.
;;; TODO: define constructors, type predicates, and accessors
;;;       for the defined set of bard atomic-structures.

;;; the defined atomic structures
;;; for each of these bard needs:
;;; - a constructor
;;; - a type predicate
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



