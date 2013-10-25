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

(defclass <globals> ()
  ((variables :accessor variables
             :initform (make-array 256 :adjustable t :initial-element (undefined)))))

(defun init-globals ()
  (setf *bard-globals* (make-instance '<globals>)))

(defmethod get-global ((i integer))
  (aref (variables *bard-globals*) i))

(defmethod set-global! ((i integer) val)
  (setf (aref (variables *bard-globals*) i) val)
  val)

