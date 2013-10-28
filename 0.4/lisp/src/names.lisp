;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          names.lisp
;;;; Project:       Bard
;;;; Purpose:       bard symbols and keywords
;;;; Author:        mikel evins, after Norvig
;;;;
;;;; ***********************************************************************

(in-package :bard)

(defclass <name> ()
  ((name :accessor name :initarg :name)))

(defclass <symbol> (<name>)
  ((module :accessor module :initarg :module)))

(defmethod print-object ((sym <symbol>)(s stream))
  (format s "~a:~a" (module sym)(name sym)))

(defun make-bard-symbol (name)
  (make-instance '<symbol> :name name :module nil))

(defclass <keyword> (<name>)())

(defmethod print-object ((k <keyword>)(s stream))
  (format s "~a:" (name k)))

(defun make-bard-keyword (name)
  (make-instance '<keyword> :name name))
