;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          base-singletons.lisp
;;;; Project:       Bard
;;;; Purpose:       the base built-in named constants
;;;; Author:        mikel evins
;;;; Copyright:     2013 mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard)

;;; ---------------------------------------------------------------------
;;; end
;;; ---------------------------------------------------------------------

(defclass <end> ()()
  (:metaclass org.tfeb.hax.singleton-classes:singleton-class))

(defparameter *end* (make-instance '<end>))

(defun end () *end*)

(defmethod end? (x)(declare (ignore x)) nil)
(defmethod end? ((x <end>)) t)

;;; ---------------------------------------------------------------------
;;; <undefined>
;;; ---------------------------------------------------------------------

(defclass <undefined> ()()
  (:metaclass org.tfeb.hax.singleton-classes:singleton-class))

(defparameter *undefined* (make-instance '<undefined>))

(defun undefined () *undefined*)

(defmethod undefined? (x)(declare (ignore x)) nil)
(defmethod undefined? ((x <undefined>)) t)
(defun defined? (x)(not (undefined? x)))

;;; ---------------------------------------------------------------------
;;; nothing
;;; ---------------------------------------------------------------------

(defclass <nothing> ()()
  (:metaclass org.tfeb.hax.singleton-classes:singleton-class))

(defparameter *nothing* (make-instance '<nothing>))
(defun nothing () *nothing*)

(defmethod nothing? (x)(declare (ignore x)) nil)
(defmethod nothing? ((x <nothing>)) t)

(defun something? (x)(not (nothing? x)))

;;; ---------------------------------------------------------------------
;;; booleans
;;; ---------------------------------------------------------------------

(defclass <boolean> ()()
  (:metaclass org.tfeb.hax.singleton-classes:singleton-class))

(defmethod boolean? (x)(declare (ignore x)) nil)
(defmethod boolean? ((x <boolean>)) t)

(defclass <false> (<boolean>)()
  (:metaclass org.tfeb.hax.singleton-classes:singleton-class))

(defparameter *false* (make-instance '<false>))
(defun false () *false*)

(defclass <true> (<boolean>)()
  (:metaclass org.tfeb.hax.singleton-classes:singleton-class))

(defparameter *true* (make-instance '<true>))
(defun true () *true*)

(defmethod true? (x)(declare (ignore x)) t)
(defmethod true? ((x <nothing>)) nil)
(defmethod true? ((x <false>)) nil)

(defmethod false? (x)(declare (ignore x)) nil)
(defmethod false? ((x <nothing>)) t)
(defmethod false? ((x <false>)) t)

;;; ---------------------------------------------------------------------
;;; 
;;; ---------------------------------------------------------------------

(defmethod base-singleton? (x)(declare (ignore x)) nil)
(defmethod base-singleton? ((x <end>)) t)
(defmethod base-singleton? ((x <undefined>)) t)
(defmethod base-singleton? ((x <nothing>)) t)
(defmethod base-singleton? ((x <true>)) t)
(defmethod base-singleton? ((x <false>)) t)
