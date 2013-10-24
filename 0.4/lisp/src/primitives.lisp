;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          primitives.lisp
;;;; Project:       Bard
;;;; Purpose:       representation of Bard primitives
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard)

(defmethod primitive? (x env argcount)
  (declare (ignore x))
  nil)


