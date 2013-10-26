;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          methods.lisp
;;;; Project:       Bard
;;;; Purpose:       implementation of method functions
;;;; Author:        mikel evins, after Norvig
;;;;
;;;; ***********************************************************************

(in-package :bard)

;;; ---------------------------------------------------------------------
;;; method-functions
;;; ---------------------------------------------------------------------

(defun print-mfn (mfn &optional (stream *standard-output*) depth)
  (declare (ignore depth))
  (if (mfn-name mfn)
      (format stream "#<method-function>{name: ~a}" (mfn-name mfn))
      (format stream "#<method-function>")))

(defstruct (mfn (:print-function print-mfn))
  code (env nil) (name nil) (args nil))

(defun new-mfn (&key code env name args)
  (assemble (make-mfn :env env :name name :args args
                      :code (optimize code))))


