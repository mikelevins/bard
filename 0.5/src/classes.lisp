;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          classes.lisp
;;;; Project:       Bard
;;;; Purpose:       representation of Bard classes
;;;; Author:        mikel evins
;;;; Copyright:     2013 mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard)

;;; ---------------------------------------------------------------------
;;; Bard classes
;;; ---------------------------------------------------------------------

(defparameter Accessor (make-instance '|<class>| :name 'bard::|Accessor|))
(defparameter Actor (make-instance '|<class>| :name 'bard::|Actor|))
(defparameter Anything (make-instance '|<class>| :name 'bard::|Anything|))
(defparameter Array (make-instance '|<class>| :name 'bard::|Array|))
(defparameter Atom (make-instance '|<class>| :name 'bard::|Atom|))
(defparameter Bard (make-instance '|<class>| :name 'bard::|Bard|))
(defparameter Boolean (make-instance '|<class>| :name 'bard::|Boolean|))
(defparameter Character (make-instance '|<class>| :name 'bard::|Character|))
(defparameter Class (make-instance '|<class>| :name 'bard::|Class|))
(defparameter Collection (make-instance '|<class>| :name 'bard::|Collection|))
(defparameter Complex (make-instance '|<class>| :name 'bard::|Complex|))
(defparameter Condition (make-instance '|<class>| :name 'bard::|Condition|))
(defparameter EndOfFile (make-instance '|<class>| :name 'bard::|EndOfFile|))
(defparameter Float (make-instance '|<class>| :name 'bard::|Float|))
(defparameter Function (make-instance '|<class>| :name 'bard::|Function|))
(defparameter Gatherer (make-instance '|<class>| :name 'bard::|Gatherer|))
(defparameter Generator (make-instance '|<class>| :name 'bard::|Generator|))
(defparameter Getter (make-instance '|<class>| :name 'bard::|Getter|))
(defparameter Integer (make-instance '|<class>| :name 'bard::|Integer|))
(defparameter Keyword (make-instance '|<class>| :name 'bard::|Keyword|))
(defparameter List (make-instance '|<class>| :name 'bard::|List|))
(defparameter Macro (make-instance '|<class>| :name 'bard::|Macro|))
(defparameter Map (make-instance '|<class>| :name 'bard::|Map|))
(defparameter Method (make-instance '|<class>| :name 'bard::|Method|))
(defparameter Mutable (make-instance '|<class>| :name 'bard::|Mutable|))
(defparameter MutableMap (make-instance '|<class>| :name 'bard::|MutableMap|))
(defparameter Name (make-instance '|<class>| :name 'bard::|Name|))
(defparameter Number (make-instance '|<class>| :name 'bard::|Number|))
(defparameter Ordered (make-instance '|<class>| :name 'bard::|Ordered|))
(defparameter Pair (make-instance '|<class>| :name 'bard::|Pair|))
(defparameter Procedure (make-instance '|<class>| :name 'bard::|Procedure|))
(defparameter Ratio (make-instance '|<class>| :name 'bard::|Ratio|))
(defparameter Rational (make-instance '|<class>| :name 'bard::|Rational|))
(defparameter Real (make-instance '|<class>| :name 'bard::|Real|))
(defparameter ResourceName (make-instance '|<class>| :name 'bard::|ResourceName|))
(defparameter Set (make-instance '|<class>| :name 'bard::|Set|))
(defparameter Setter (make-instance '|<class>| :name 'bard::|Setter|))
(defparameter Stream (make-instance '|<class>| :name 'bard::|Steam|))
(defparameter Structure (make-instance '|<class>| :name 'bard::|Structure|))
(defparameter Symbol (make-instance '|<class>| :name 'bard::|Symbol|))
(defparameter Text (make-instance '|<class>| :name 'bard::|Text|))
(defparameter Type (make-instance '|<class>| :name 'bard::|Type|))
(defparameter Undefined (make-instance '|<class>| :name 'bard::|Undefined|))
(defparameter UniformVector (make-instance '|<class>| :name 'bard::|UniformVector|))
(defparameter Vector (make-instance '|<class>| :name 'bard::|Vector|))
(defparameter Word (make-instance '|<class>| :name 'bard::|Word|))


