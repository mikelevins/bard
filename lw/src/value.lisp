;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          value.lisp
;;;; Project:       Bard
;;;; Purpose:       base value definitions
;;;; Author:        mikel evins
;;;; Copyright:     2011 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard)

(defclass undefined ()()(:metaclass singleton-class))
(defun undefined ()(make-instance 'undefined))

(defclass nothing ()()(:metaclass singleton-class))
(defun nothing ()(make-instance 'nothing))

(defclass true ()()(:metaclass singleton-class))
(defun true ()(make-instance 'true))

(defclass false ()()(:metaclass singleton-class))
(defun false ()(make-instance 'false))

(defclass integer ()(integer-value))

(defclass float ()(float-value))

(defclass character ()(data))

(defclass name ()(module-name text))

(defclass sequence ()(elements))

(defclass text ()(data))

(defclass map ()(entries))

(defclass primitive ()(debug-name arg-count code))

(defclass method ()(debug-name signature code))

(defclass function ()(debug-name signature))

(defclass module ()(module-name export-list variable-table))

