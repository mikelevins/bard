;;;; -*- Mode: Lisp; Syntax: Common-Lisp -*-
;;;; ---------------------------------------------------------------------
;;;; bardvm
;;;; A VM implementation based on Norvig's Scheme compiler from PAIP
;;;; fn.lisp
;;;; representing bard functions
;;;; ---------------------------------------------------------------------
;;;; Code from Paradigms of Artificial Intelligence Programming
;;;; Copyright (c) 1991 Peter Norvig
;;;; integrated into bardvm by mikel evins, mikel@evins.net, July 2020

(in-package :bardvm)

;;;; ---------------------------------------------------------------------
;;;; fn structure
;;;; ---------------------------------------------------------------------

(defstruct (fn (:print-function print-fn))
  code (env nil) (name nil) (args nil))

(defun print-fn (fn &optional (stream *standard-output*) depth)
  (declare (ignore depth))
  (format stream "{~a}" (or (fn-name fn) '??)))

(defun name! (fn name)
  "Set the name field of fn, if it is an un-named fn."
  (when (and (fn-p fn) (null (fn-name fn)))
    (setf (fn-name fn) name))
  name)
