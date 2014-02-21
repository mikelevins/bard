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

(defclass procedure ()
  ((code :accessor code :initarg :code)
   (env :accessor env :initform nil :initarg :env)
   (name :accessor name :initform nil :initarg :name)
   (args :accessor args :initform nil :initarg :args)))

(defmethod print-object ((proc procedure)(out stream))
  (print-unreadable-object (proc out :type t :identity t)
    (format out "~a" (or (name proc)
                         "(anonymous)"))))

(defmethod procedure-p (x)
  (declare (ignore x))
  nil)

(defmethod procedure-p ((x procedure))
  (declare (ignore x))
  t)

(defun new-procedure (&key (type 'procedure) code env name args)
  (assemble (make-instance type :env env :name name :args args
                           :code (optimize code))))

(defmethod show-procedure (thing &optional (stream *standard-output*) indent)
  (declare (ignore indent))
  (format stream "~8s" thing))

(defmethod show-procedure ((proc procedure) &optional (stream *standard-output*) (indent 2))
  (progn
    (fresh-line)
    (dotimes (i (length (code proc)))
      (let ((instr (elt (code proc) i)))
        (if (label-p instr)
            (format stream "~a:" instr)
            (progn
              (format stream "~VT~2d: " indent i)
              (dolist (arg instr)
                (show-procedure arg stream (+ indent 8)))
              (fresh-line)))))))

;;; ---------------------------------------------------------------------

(defclass method (procedure)())
;;; ---------------------------------------------------------------------

(defclass function (procedure)())
