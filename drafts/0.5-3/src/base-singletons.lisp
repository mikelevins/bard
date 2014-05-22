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
;;; undefined
;;; ---------------------------------------------------------------------

(defclass undefined ()()
  (:metaclass org.tfeb.hax.singleton-classes:singleton-class))

(defparameter *undefined* (make-instance 'undefined))

(defun undefined () *undefined*)

(defmethod undefined? (x)(declare (ignore x)) nil)
(defmethod undefined? ((x undefined)) t)
(defun defined? (x)(not (undefined? x)))

;;; ---------------------------------------------------------------------
;;; nothing
;;; ---------------------------------------------------------------------

(defparameter *nothing* nil)

(defun nothing () *nothing*)

(defmethod nothing? (x)(declare (ignore x)) nil)
(defmethod nothing? ((x null)) t)
(defun something? (x)(not (nothing? x)))

;;; ---------------------------------------------------------------------
;;; true
;;; ---------------------------------------------------------------------

(defclass true ()()
  (:metaclass org.tfeb.hax.singleton-classes:singleton-class))

(defparameter *true* (make-instance 'true))

(defun true () *true*)

(defmethod true? (x)(declare (ignore x)) t)

;;; ---------------------------------------------------------------------
;;; false
;;; ---------------------------------------------------------------------

(defclass false ()()
  (:metaclass org.tfeb.hax.singleton-classes:singleton-class))

(defparameter *false* (make-instance 'false))

(defun false () *false*)

(defmethod false? (x)(declare (ignore x)) nil)
(defmethod false? ((x false))(declare (ignore x)) t)
(defmethod false? ((x undefined))(declare (ignore x)) t)
(defmethod false? ((x null))(declare (ignore x)) t)

;;; ---------------------------------------------------------------------
;;; end
;;; ---------------------------------------------------------------------

(defclass end ()()
  (:metaclass org.tfeb.hax.singleton-classes:singleton-class))

(defparameter *end* (make-instance 'end))

(defun end () *end*)

(defmethod end? (x)(declare (ignore x)) nil)
(defmethod end? ((x end)) t)

