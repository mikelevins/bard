;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          name.lisp
;;;; Project:       Bard
;;;; Purpose:       Bard names (symbols, keywords, locators)
;;;; Author:        mikel evins
;;;; Copyright:     2014 mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard)


;;; ---------------------------------------------------------------------
;;; name class
;;; ---------------------------------------------------------------------

(defclass name ()())

;;; ---------------------------------------------------------------------
;;; keywords
;;; ---------------------------------------------------------------------

(defclass keyword (name)
  ((name :accessor keyword-name :initarg :name)))

;;; ---------------------------------------------------------------------
;;; symbols
;;; ---------------------------------------------------------------------

(defclass symbol (name)
  ((name :accessor symbol-name :initarg :name)
   (module :accessor symbol-module :initarg :module)))

(defmethod print-object ((obj symbol)(out stream))
  (let ((module (symbol-module obj)))
    (unless (equal module (current-module))
      (princ (module-name module) out)
      (princ #\: out))
    (princ (symbol-name obj) out)))


