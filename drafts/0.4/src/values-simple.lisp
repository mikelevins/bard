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
;;; octet
;;; ---------------------------------------------------------------------

(defmethod octet? (x)
  (declare (ignore x))
  nil)

(defmethod octet? ((x integer))
  (<= 0 x 255))

(deftype octet ()
  `(satisfies octet?))

;;; ---------------------------------------------------------------------
;;; eof
;;; ---------------------------------------------------------------------

(defclass <eof> ()())

(defparameter *eof* (make-instance '<eof>))

(defun eof () *eof*)

(defmethod eof? (x)(declare (ignore x)) nil)
(defmethod eof? ((x <eof>)) t)

(defmethod get-structure ((x <eof>))
  (declare (ignore x))
  *eof-structure*)

;;; ---------------------------------------------------------------------
;;; <undefined>
;;; ---------------------------------------------------------------------

(defclass <undefined> ()())

(defparameter *undefined* (make-instance '<undefined>))

(defun undefined () *undefined*)

(defmethod undefined? (x)(declare (ignore x)) nil)
(defmethod undefined? ((x <undefined>)) t)
(defun defined? (x)(not (undefined? x)))

(defmethod get-structure ((x <undefined>))
  (declare (ignore x))
  *undefined-structure*)

;;; ---------------------------------------------------------------------
;;; nothing
;;; ---------------------------------------------------------------------

(defun nothing () nil)
(defparameter *nothing* nil)

(defmethod nothing? (x)(declare (ignore x)) nil)
(defmethod nothing? ((x null)) t)

(defun something? (x)(not (nothing? x)))

(defmethod get-structure ((x null))
  (declare (ignore x))
  *null-structure*)

;;; ---------------------------------------------------------------------
;;; booleans
;;; ---------------------------------------------------------------------

(defclass <boolean> ()())

(defmethod boolean? (x)(declare (ignore x)) nil)
(defmethod boolean? ((x <boolean>)) t)

(defclass <false> (<boolean>)())
(defparameter *false* (make-instance '<false>))
(defun false () *false*)

(defclass <true> (<boolean>)())
(defparameter *true* (make-instance '<true>))
(defun true () *true*)

(defmethod true? (x)(declare (ignore x)) t)
(defmethod true? ((x null)) nil)
(defmethod true? ((x <false>)) nil)

(defmethod false? (x)(declare (ignore x)) nil)
(defmethod false? ((x null)) t)
(defmethod false? ((x <false>)) t)

(defmethod get-structure ((x <true>))
  (declare (ignore x))
  *boolean-structure*)

(defmethod get-structure ((x <false>))
  (declare (ignore x))
  *boolean-structure*)

