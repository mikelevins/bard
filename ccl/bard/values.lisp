;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          values.lisp
;;;; Project:       Bard
;;;; Purpose:       representations of Bard values
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(cl:in-package #:bard)

;;; ---------------------------------------------------------------------
;;; Anything
;;; ---------------------------------------------------------------------

;;; ---------------------------------------------------------------------
;;; Undefined
;;; ---------------------------------------------------------------------

;;; ---------------------------------------------------------------------
;;; Type
;;; ---------------------------------------------------------------------

;;; ---------------------------------------------------------------------
;;; Protocol
;;; ---------------------------------------------------------------------

;;; ---------------------------------------------------------------------
;;; Singleton
;;; ---------------------------------------------------------------------

;;; ---------------------------------------------------------------------
;;; Null
;;; ---------------------------------------------------------------------

(cl:defparameter nothing (fset:empty-seq))

(cl:defun %is-nothing? (x)(cl:eq x nothing))

(cl:deftype Null () `(cl:satisfies %is-nothing?))

;;; ---------------------------------------------------------------------
;;; Boolean
;;; ---------------------------------------------------------------------

(cl:deftype Boolean () 'cl:boolean)

(cl:defparameter true cl:t)

(cl:defun %is-true? (x)(cl:eq x true))

(cl:deftype True () `(cl:satisfies %is-true?))

(cl:defparameter false cl:nil)

(cl:defun %is-false? (x)(cl:eq x false))

(cl:deftype False () `(cl:satisfies %is-false?))


;;; ---------------------------------------------------------------------
;;; Character
;;; ---------------------------------------------------------------------

;;; ---------------------------------------------------------------------
;;; Number
;;; ---------------------------------------------------------------------

;;; ----------------------------------------------------------------------
;;; Name
;;; ----------------------------------------------------------------------

;;; ----------------------------------------------------------------------
;;; Text
;;; ----------------------------------------------------------------------

;;; ----------------------------------------------------------------------
;;; List
;;; ----------------------------------------------------------------------

(cl:defun %is-list? (x)(cl:typep x 'fset:seq))

(cl:deftype List () `(cl:satisfies %is-list?))

;;; ---------------------------------------------------------------------
;;; Frame
;;; ---------------------------------------------------------------------

(cl:defun %is-frame? (x)(cl:typep x 'fset:wb-map))

(cl:deftype Frame () `(cl:satisfies %is-frame?))

;;; ---------------------------------------------------------------------
;;; Method
;;; ---------------------------------------------------------------------

(cl:defclass method ()
  (lambda-list method-functio debug-name))

;;; ---------------------------------------------------------------------
;;; Function
;;; ---------------------------------------------------------------------

(cl:defclass function ()
  (inputs outputs methods debug-name))

;;; ---------------------------------------------------------------------
;;; Stream
;;; ---------------------------------------------------------------------

