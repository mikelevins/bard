;;;; ***********************************************************************
;;;;
;;;; Name:          package.lisp
;;;; Project:       the bard programming language
;;;; Purpose:       package definitions
;;;; Author:        mikel evins
;;;; Copyright:     2024 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (eclector.readtable:readtable-case eclector.readtable:*readtable*)
        :preserve))
(defpackage :bard.internal
  (:use :cl)
  (:shadow cl:read-from-string))


