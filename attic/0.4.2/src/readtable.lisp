;;;; ***********************************************************************
;;;;
;;;; Name:          package.lisp
;;;; Project:       the Bard language
;;;; Purpose:       package definitions
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard.core)

(defparameter *bard-readtable*
  (let ((tbl (copy-readtable)))
    (setf (readtable-case tbl) :preserve)
    tbl))
