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
  ((expression :accessor mfn-expression :initform nil :initarg :expression)
   (code :accessor mfn-code :initform nil :initarg :code)
   (env :accessor mfn-env :initform nil :initarg :env)
   (name :accessor mfn-name :initform nil :initarg :name)
   (args :accessor mfn-args :initform nil :initarg :args)))


