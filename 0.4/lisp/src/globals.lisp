;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          globals.lisp
;;;; Project:       Bard
;;;; Purpose:       bard modules
;;;; Author:        mikel evins
;;;; Copyright:     2013 mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard)

;;; ---------------------------------------------------------------------
;;; ABOUT
;;; ---------------------------------------------------------------------
;;; All globals are defined in a single per-VM array.  Globals are
;;; accessible only by index into this array.  Modules provide a layer
;;; above globals that provide namespaces and per-namespace named keys
;;; to the globals array.

(defparameter *bard-globals* nil)

(defun init-globals ()
  (setf *bard-globals* (make-array 256 :fill-pointer 0 :adjustable t :initial-element *undefined*)))

(defun globals-capacity ()
  (first (array-dimensions *bard-globals*)))

(defun globals-count ()
  (length *bard-globals*))

(defun add-global! (&optional (val *undefined*))
  (vector-push-extend val *bard-globals*)
  (globals-count))

(defmethod get-global ((i integer))
  (aref *bard-globals* i))

(defmethod set-global! ((i integer) val)
  (setf (aref *bard-globals* i) val)
  val)

