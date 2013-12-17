;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          box.lisp
;;;; Project:       Bard
;;;; Purpose:       bard's format for storing compiled code
;;;; Author:        mikel evins
;;;; Copyright:     2013 mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard)

;;; ---------------------------------------------------------------------
;;; <box>
;;; ---------------------------------------------------------------------
;;; a box is an object that contains Bard code. It can store a source 
;;; form, the bardo object format for the source form, and the
;;; native-compiled thunk for the form.

(defparameter *box-version* (vector 1 0 0 0))

(defclass <box> ()
  ((box-version :reader box-version :initform *box-version*)
   (source-form :accessor source-form :initform nil :initarg :source-form)
   (bardo-form :accessor bardo-form :initform nil :initarg :bardo-form)
   ;; we never save the native form; we always regenerate it from the bardo-form
   (native-form :accessor native-form :initform nil :initarg :native-form)))

