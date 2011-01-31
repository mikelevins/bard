;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          functions.lisp
;;;; Project:       folio - the Bard runtime
;;;; Purpose:       convenient shorthand for anonymous functions
;;;; Author:        mikel evins
;;;; Copyright:     2010 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(defpackage "FOLIO.FN"
  (:use :cl)
  (:nicknames "FN")
  (:export "$" "FN"))

(in-package :FN)


;;; ---------------------------------------------------------------------
;;; fn
;;; ---------------------------------------------------------------------

(defmacro fn (args &body body)
  `(lambda ,args ,@body))

(defmacro ^ (args &body body)
  `(lambda ,args ,@body))

(defmacro $ (f &rest args)
  `(funcall ,f ,@args))