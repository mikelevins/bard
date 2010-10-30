;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          values.lisp
;;;; Project:       Bard - a modern Lisp
;;;; Purpose:       basic Bard values
;;;; Author:        mikel evins
;;;; Copyright:     2010 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :|bard-internal|)

;;; ============================================================
;;; common operations
;;; ============================================================

(defmethod = (x y)(cl:eql x y))

;;; ============================================================
;;; base value types
;;; ============================================================

;;; ------------------------------------------------------------
;;; Nothing
;;; ------------------------------------------------------------

(defclass nothing ()()(:metaclass singleton-class))

(defmethod print-object ((n nothing)(s stream))
  (format s "nothing"))

(defun nothing ()(make-instance 'nothing))

(defmethod nothing? (x)(declare (ignore x)) nil)
(defmethod nothing? ((x nothing))(declare (ignore x)) t)
(defun something? (x)(not (nothing? x)))

(defmethod = ((x nothing) y)
  (declare (ignore y))
  nil)

(defmethod = (x (y nothing))
  (declare (ignore x))
  nil)

(defmethod = ((x nothing) (y nothing))
  (declare (ignore x y))
  t)

;;; ------------------------------------------------------------
;;; Booleans
;;; ------------------------------------------------------------

(defclass boolean ()())

;;; true

(defclass true (boolean)()(:metaclass singleton-class))

(defmethod print-object ((tt true)(s stream))
  (format s "true"))

(defun true ()(make-instance 'true))

(defmethod true? (x)(declare (ignore x)) nil)
(defmethod true? ((x true))(declare (ignore x)) t)

(defmethod = ((x true) y)
  (declare (ignore y))
  nil)

(defmethod = (x (y true))
  (declare (ignore x))
  nil)

(defmethod = ((x true) (y true))
  (declare (ignore x y))
  t)

;;; false

(defclass false (boolean)()(:metaclass singleton-class))

(defmethod print-object ((f false)(s stream))
  (format s "false"))

(defun false ()(make-instance 'false))

(defmethod false? (x)(declare (ignore x)) nil)
(defmethod false? ((x false))(declare (ignore x)) t)

(defmethod = ((x false) y)
  (declare (ignore y))
  nil)

(defmethod = (x (y false))
  (declare (ignore x))
  nil)

(defmethod = ((x false) (y false))
  (declare (ignore x y))
  t)

