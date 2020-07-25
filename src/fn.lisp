;;;; -*- Mode: Lisp; Syntax: Common-Lisp -*-
;;;; ---------------------------------------------------------------------
;;;; bardvm
;;;; A VM implementation based on Norvig's Scheme compiler from PAIP
;;;; fn.lisp
;;;; representing bard functions
;;;; ---------------------------------------------------------------------

(in-package :bardvm)

;;;; ---------------------------------------------------------------------
;;;; fn structure
;;;; ---------------------------------------------------------------------

(defstruct (fn (:print-function print-fn))
  code (env nil) (name nil) (args nil))

(defun print-fn (fn &optional (stream *standard-output*) depth)
  (declare (ignore depth))
  (if (fn-name fn)
      (format stream "#<method ~S>" (fn-name fn))
      (format stream "#<an anonymous method>")))

(defun name! (fn name)
  "Set the name field of fn, if it is an un-named fn."
  (when (and (fn-p fn) (null (fn-name fn)))
    (setf (fn-name fn) name))
  name)
