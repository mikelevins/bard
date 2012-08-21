;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          schema.scm
;;;; Project:       Bard VM
;;;; Purpose:       implementation of record-like data representations
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

;;; a bard datatype is a tuple of values.  users can create new
;;; datatypes by defining schemas.  a schema describes a tuple of
;;; values, and defines functions for creating them and acessing their
;;; elements. elements of a schema instsance may be accessed by name
;;; or by index, depending on the schema's definition.  schemas whose
;;; elements are accessed by name are called tables; those whose
;;; elements are accessed by index are called vectors. the match
;;; special form provides syntax for binding variables to the elements
;;; of a schema instance.
;;;
;;; <table> is a built-in table schema that you can use to construct
;;; general-use record-like data structures without defining a new
;;; schema. Similarly, Bard provides several built-in vector schemas:
;;; <vector>, <bitvector>, and a whole family of homogeneous vectors
;;; of machine words, including <u8vector>, <s8vector>, <u32vector>,
;;; and so on.

(define (slotref obj slot-name)(not-yet-implemented))
(define (slot-setter obj slot-name)(not-yet-implemented))
