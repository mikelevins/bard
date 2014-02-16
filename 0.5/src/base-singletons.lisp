;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          base-singletons.lisp
;;;; Project:       Bard
;;;; Purpose:       unique primitive values
;;;; Author:        mikel evins
;;;; Copyright:     2014 mikel evins
;;;;                Incrementally derived from Peter Norvig's Scheme compiler
;;;;                Code from Paradigms of Artificial Intelligence Programming
;;;;                Copyright (c) 1991 Peter Norvig
;;;;
;;;; ***********************************************************************

(in-package :bard)

;;; ---------------------------------------------------------------------
;;; nothing
;;; ---------------------------------------------------------------------

;;; ---------------------------------------------------------------------
;;; true
;;; ---------------------------------------------------------------------

;;; ---------------------------------------------------------------------
;;; false
;;; ---------------------------------------------------------------------

;;; ---------------------------------------------------------------------
;;; eof
;;; ---------------------------------------------------------------------

;;; ---------------------------------------------------------------------
;;; undefined
;;; ---------------------------------------------------------------------

(defclass undefined ()()
  (:metaclass org.tfeb.hax.singleton-classes:singleton-class))

(defparameter *undefined* (make-instance 'undefined))

(defun undefined () *undefined*)

(defmethod undefined? (x)(declare (ignore x)) nil)
(defmethod undefined? ((x undefined)) t)
(defun defined? (x)(not (undefined? x)))

