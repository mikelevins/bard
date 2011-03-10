;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          value.lisp
;;;; Project:       Bard
;;;; Purpose:       base value definitions
;;;; Author:        mikel evins
;;;; Copyright:     2011 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard)

;;; ---------------------------------------------------------------------
;;; undefined
;;; ---------------------------------------------------------------------

(defclass undefined ()()(:metaclass singleton-class))
(defun undefined ()(make-instance 'undefined))

(defmethod print-object ((obj undefined)(s stream))
  (princ "undefined" s))

;;; ---------------------------------------------------------------------
;;; nothing
;;; ---------------------------------------------------------------------

(defclass nothing ()()(:metaclass singleton-class))
(defun nothing ()(make-instance 'nothing))

(defmethod print-object ((obj nothing)(s stream))
  (princ "nothing" s))

;;; ---------------------------------------------------------------------
;;; true and false
;;; ---------------------------------------------------------------------

(defclass true ()()(:metaclass singleton-class))
(defun true ()(make-instance 'true))

(defmethod print-object ((obj true)(s stream))
  (princ "true" s))

(defclass false ()()(:metaclass singleton-class))
(defun false ()(make-instance 'false))

(defmethod print-object ((obj false)(s stream))
  (princ "false" s))

;;; ---------------------------------------------------------------------
;;; numbers
;;; ---------------------------------------------------------------------

(defclass integer ()((data :reader data :initarg :data)))

(defmethod print-object ((i integer)(s stream))
  (format s "~A" (data i)))

(defclass float ()((data :reader data :initarg :data)))

(defmethod print-object ((f float)(s stream))
  (format s "~A" (data f)))

;;; ---------------------------------------------------------------------
;;; characters
;;; ---------------------------------------------------------------------

(defclass character ()((data :reader data :initarg :data)))

(defmethod print-object ((ch character)(s stream))
  (princ #\\ s)(princ (print-name-for (data ch)) s))

;;; ---------------------------------------------------------------------
;;; modules and names
;;; ---------------------------------------------------------------------

(defclass module ()(module-name export-list variable-table))

(defclass name ()
  ((module-name :reader module-name :initarg :module-name)
   (variable-name :reader variable-name :initarg :variable-name)))

(defmethod print-object ((nm name)(s stream))
  (when (module-name nm)
    (unless (string= "bard.keyword" (module-name nm))
      (princ (module-name nm) s))
    (princ ":" s))
  (princ (variable-name nm) s))

;;; ---------------------------------------------------------------------
;;; sequences and maps
;;; ---------------------------------------------------------------------

(defclass sequence ()(elements))

(defclass text ()((data :reader data)))
(defmethod initialize-instance ((tx text) &rest initargs &key data &allow-other-keys)
  (assert (and data (stringp data))()
          "You must supply string data to create a new text object")
  (setf (slot-value tx 'data)
        (fset:convert 'fset:seq data)))

(defmethod print-object ((tx text)(s stream))
  (princ #\" s)
  (fset:do-seq (ch (data tx))
    (princ ch s))
  (princ #\" s))

(defclass map ()(entries))

;;; ---------------------------------------------------------------------
;;; functions
;;; ---------------------------------------------------------------------

(defclass primitive ()(debug-name arg-count code))

(defclass method ()(debug-name signature code))

(defclass function ()(debug-name signature))

;;; ---------------------------------------------------------------------
;;; types
;;; ---------------------------------------------------------------------

(defclass primitive-type ()(debug-name))

(defclass vector-type ()(debug-name))

(defclass record-type ()(debug-name))

(defclass structure-type ()(debug-name))

