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

<<<<<<< HEAD
(defclass <globals> ()
  ((variables :accessor variables
             :initform (make-array 256 :adjustable t :initial-element (undefined)))))
=======
(defmethod next-global-id ((gs <globals>))
  (let ((id (next-id gs)))
    (incf (next-id gs))
    id))
>>>>>>> d6057b4c4644665b80e79c88cb3a3480ae368b64

(defmethod get-global ((gs <globals>)(i integer))
  (aref (variables gs) i))

(defmethod set-global! ((gs <globals>)(i integer) val)
  (setf (aref (variables gs) i) val)
  val)
<<<<<<< HEAD
=======

(defun make-standard-globals ()
  (make-instance '<globals>))
>>>>>>> d6057b4c4644665b80e79c88cb3a3480ae368b64
