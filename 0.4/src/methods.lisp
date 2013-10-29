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

(defclass <mfn> ()
  ((code :accessor mfn-code :initform nil :initarg :code)
   (env :accessor mfn-env :initform nil :initarg :env)
   (name :accessor mfn-name :initform nil :initarg :name)
   (args :accessor mfn-args :initform nil :initarg :args)))

(defmethod print-object ((mfn <mfn>) (s stream))
  (show mfn s))

(defmethod show ((mfn <mfn>) &optional (stream *standard-output*))
  (let ((mname (mfn-name mfn))
        (code (mfn-code mfn)))
    (format stream "#<method>{~%")
    (when mname
      (format stream "name: ~a " mname))
    (show code stream)
    (format stream "}~%")))

