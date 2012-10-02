;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          values.lisp
;;;; Project:       The Bard Programming Language
;;;; Purpose:       representation of Bard values
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

;;;; ---------------------------------------------------------------------
;;;; ABOUT
;;;; ---------------------------------------------------------------------
;;;; I start out by defining all Bard values as instances of CLOS classes.
;;;; This makes things simple, and I can make changes for the sake of
;;;; optimization later.

(in-package :bard)

;;; ---------------------------------------------------------------------
;;; Abstract Classes
;;; ---------------------------------------------------------------------

;;; traits

(defclass Ordered ()())
(defclass Equal ()())
(defclass Keyed ()())

;;; types

(defclass Anything ()())
(defclass Undefined (Anything)())
(defclass Atom (Anything)())
(defclass Collection (Anything)())
(defclass Nothing (Atom Collection Equal)())
(defclass Boolean (Atom Equal)())
(defclass Character (Atom Equal Ordered)())
(defclass Number (Atom Equal Ordered)())
(defclass Name (Atom Equal Ordered)())
(defclass Applicable (Atom)())
(defclass List (Collection Ordered Keyed)())
(defclass Map (Collection Keyed)())



;;; ---------------------------------------------------------------------
;;; Concrete Schemas
;;; ---------------------------------------------------------------------

;;; ---------------------------------------------------------------------
;;; Protocols
;;; ---------------------------------------------------------------------
