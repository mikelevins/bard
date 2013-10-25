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

(defmethod next-global-id ((gs <globals>))
  (let ((id (next-id gs)))
    (incf (next-id gs))
    id))

(defmethod get-global ((gs <globals>)(i integer))
  (aref (variables gs) i))

(defmethod set-global! ((gs <globals>)(i integer) val)
  (setf (aref (variables gs) i) val)
  val)

(defun make-standard-globals ()
  (make-instance '<globals>))
