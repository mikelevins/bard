;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          procedures.lisp
;;;; Project:       Bard
;;;; Purpose:       representation of bard procedures
;;;; Author:        mikel evins
;;;; Copyright:     2014 mikel evins
;;;;                Incrementally derived from Peter Norvig's Scheme compiler
;;;;                Code from Paradigms of Artificial Intelligence Programming
;;;;                Copyright (c) 1991 Peter Norvig
;;;;
;;;; ***********************************************************************

(in-package :bard)

;;; TODO:
;;; 1. add representation of (generic) functions
;;; 2. add dispatch
;;; 3. modify method representation so that it
;;;    can optionally contain a native-code version
;;;    of the compiled method

(defstruct (method (:print-function print-method))
  code (env nil) (name nil) (args nil))

(defun print-method (method &optional (stream *standard-output*) depth)
  (declare (ignore depth))
  (format stream "{~a}" (or (method-name method) '??)))

(defun new-method (&key code env name args)
  "Build a new function."
  (assemble (make-method :env env :name name :args args
                     :code (optimize code))))

(defun show-method (method &optional (stream *standard-output*) (indent 2))
  "Print all the instructions in a function.
  If the argument is not a function, just princ it, 
  but in a column at least 8 spaces wide."
  ;; This version handles code that has been assembled into a vector
  (if (not (method-p method))
      (format stream "~8a" method)
      (progn
        (fresh-line)
        (dotimes (i (length (method-code method)))
          (let ((instr (elt (method-code method) i)))
            (if (label-p instr)
                (format stream "~a:" instr)
                (progn
                  (format stream "~VT~2d: " indent i)
                  (dolist (arg instr)
                    (show-method arg stream (+ indent 8)))
                  (fresh-line))))))))
