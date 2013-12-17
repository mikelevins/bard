;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          version.lisp
;;;; Project:       Bard
;;;; Purpose:       the Bard version number
;;;; Author:        mikel evins
;;;; Copyright:     2013 mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard)

(defparameter *bard-version-number* (vector 0 5 0 "a1"))

(defparameter *bard-version-string*
  (format nil "~a.~a.~a~a"
          (elt *bard-version-number* 0)
          (elt *bard-version-number* 1)
          (elt *bard-version-number* 2)
          (elt *bard-version-number* 3)))
