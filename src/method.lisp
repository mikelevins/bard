;;;; -*- Mode: Lisp; Syntax: Common-Lisp -*-
;;;; ---------------------------------------------------------------------
;;;; bardvm
;;;; A VM implementation based on Norvig's Scheme compiler from PAIP
;;;; method.lisp
;;;; representing bard methods
;;;; ---------------------------------------------------------------------

(in-package :bardvm)

;;;; ---------------------------------------------------------------------
;;;; method structure
;;;; ---------------------------------------------------------------------

(defstruct (method (:print-function print-method))
  code (env nil) (name nil) (args nil))

(defun print-method (method &optional (stream *standard-output*) depth)
  (declare (ignore depth))
  (if (method-name method)
      (format stream "#<method ~S>" (method-name method))
      (format stream "#<an anonymous method>")))

(defun name! (method name)
  "Set the name field of method, if it is an un-named method."
  (when (and (method-p method) (null (method-name method)))
    (setf (method-name method) name))
  name)
