;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          values.lisp
;;;; Project:       Bard
;;;; Purpose:       representation of base Bard values
;;;; Author:        mikel evins
;;;; Copyright:     2013 mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard)

;;; ---------------------------------------------------------------------
;;;  unique constants
;;; ---------------------------------------------------------------------

(defclass %undefined% ()())

(defmethod print-object ((x %undefined%)(s stream))
  (princ "undefined" s))

(defparameter $undefined (make-instance '%undefined%))

(defun %undefined () $undefined)

(defmethod %undefined? (x) nil)
(defmethod %undefined? ((x %undefined%)) t)

(defmethod %nothing? (x) nil)
(defmethod %nothing? ((x null)) t)

(defclass %boolean% ()())

(defmethod %boolean? (x) nil)
(defmethod %boolean? ((x %boolean%)) t)

(defclass %true% (%boolean%)())

(defparameter $true (make-instance '%true%))

(defun %true () $true)

(defmethod %true? (x) t)
(defmethod %true? ((x null)) nil)

(defmethod print-object ((x %true%)(s stream))
  (princ "true" s))

(defclass %false% (%boolean%)())

(defparameter $false (make-instance '%false%))

(defun %false () $false)

(defun %false? (x) (not (%true? x)))
(defmethod %true? ((x %false%)) nil)

(defmethod print-object ((x %false%)(s stream))
  (princ "false" s))

(defclass %eof% ()())

(defparameter $eof (make-instance '%eof%))

(defun %eof () $eof)

(defmethod %eof? (x) nil)
(defmethod %eof? ((x %eof%)) t)

(defmethod print-object ((x %eof%)(s stream))
  (princ "eof" s))

;;; ---------------------------------------------------------------------
;;;  
;;; ---------------------------------------------------------------------
