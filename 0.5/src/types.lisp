;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          types.lisp
;;;; Project:       Bard
;;;; Purpose:       representation of concrete types
;;;; Author:        mikel evins
;;;; Copyright:     2014 mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard)

;;; ---------------------------------------------------------------------
;;; base types
;;; ---------------------------------------------------------------------

(defclass base-type ()
  ((name :reader type-name :initarg :name)))

(defclass bits-type (base-type)
  ((minimum :reader minimum :initform 0 :initarg :minimum)
   (maximum :reader maximum :initform (undefined) :initarg :maximum)))

(defclass procedure-type (base-type)())

(defclass record-type (base-type)
  ((slot-descriptions :reader slot-descriptions :initform (nothing) :initarg :slot-descriptions)))

(defclass list-type (base-type)
  ((element-type :reader element-type :initform (undefined) :initarg :element-type)
   (minimum-count :reader minimum-count :initform 0 :initarg :minimum-count)
   (maximum-count :reader maximum-count :initform (undefined) :initarg :maximum-count)))

(defclass values-type (base-type)())


(defparameter +bits+ (make-instance 'base-type :name 'bits))
(defparameter +procedure+ (make-instance 'base-type :name 'procedure))
(defparameter +record+ (make-instance 'base-type :name 'record))
(defparameter +list+ (make-instance 'base-type :name 'list))
(defparameter +values+ (make-instance 'base-type :name 'values))


