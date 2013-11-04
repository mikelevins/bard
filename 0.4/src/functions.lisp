;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          functions.lisp
;;;; Project:       Bard
;;;; Purpose:       implementation of polymorphic functions
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard)

;;; ---------------------------------------------------------------------
;;; functions
;;; ---------------------------------------------------------------------

(defclass <fn> ()
  ((input-types :accessor fn-input-types :initform nil :initarg :input-types)
   (output-types :accessor fn-output-types :initform nil :initarg :output-types)))

(defmethod print-object ((fn <fn>)(s stream))
  (format s (value->literal-string fn)))

(defun make-function (inputs outputs)
  (make-instance '<fn> :input-types inputs :output-types outputs))

(defmethod get-structure ((x <fn>))
  (declare (ignore x))
  *function-structure*)

