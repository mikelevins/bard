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

(defclass <function> ()
  ((input-types :accessor function-input-types :initform nil :initarg :input-types)
   (output-types :accessor function-output-types :initform nil :initarg :output-types)
   (method-table :accessor function-method-table :initform (make-method-table) :initarg :method-table)))

(defmethod print-object ((function <function>)(s stream))
  (format s (value->literal-string function)))

(defun make-function (inputs outputs)
  (make-instance '<function> :input-types inputs :output-types outputs))

(defmethod get-structure ((x <function>))
  (declare (ignore x))
  *function-structure*)

