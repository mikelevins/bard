;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          simple.lisp
;;;; Project:       Bard
;;;; Purpose:       representation built-in constants and simple values
;;;; Author:        mikel evins
;;;; Copyright:     2013 mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard)

;;; ---------------------------------------------------------------------
;;; eof
;;; ---------------------------------------------------------------------

(defclass <eof> ()())

(defparameter *eof* (make-instance '<eof>))

(defun eof () *eof*)

(defmethod eof? (x) nil)
(defmethod eof? ((x <eof>)) t)

;;; ---------------------------------------------------------------------
;;; <undefined>
;;; ---------------------------------------------------------------------

(defclass <undefined> ()())

(defparameter *undefined* (make-instance '<undefined>))

(defun undefined () *undefined*)

(defmethod undefined? (x) nil)
(defmethod undefined? ((x <undefined>)) t)
(defun defined? (x)(not (undefined? x)))

;;; ---------------------------------------------------------------------
;;; nothing
;;; ---------------------------------------------------------------------

(defun nothing () nil)

(defmethod nothing? (x) nil)
(defmethod nothing? ((x null)) t)

(defun something? (x)(not (nothing? x)))

;;; ---------------------------------------------------------------------
;;; booleans
;;; ---------------------------------------------------------------------

(defclass <boolean> ()())

(defmethod boolean? (x) nil)
(defmethod boolean? ((x <boolean>)) t)

(defclass <false> (<boolean>)())
(defparameter *false* (make-instance '<false>))
(defun false () *false*)

(defclass <true> (<boolean>)())
(defparameter *true* (make-instance '<true>))
(defun true () *true*)

(defmethod true? (x) t)
(defmethod true? ((x null)) nil)
(defmethod true? ((x <false>)) nil)

(defmethod false? (x) nil)
(defmethod false? ((x null)) t)
(defmethod false? ((x <false>)) t)

