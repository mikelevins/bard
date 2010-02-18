;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: asdf -*-
;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          modules.lisp
;;;; Project:       Bard - a modern Lisp
;;;; Purpose:       modules
;;;; Author:        mikel evins
;;;; Copyright:     2010 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard)

;;; ============================================================
;;; BARD modules
;;; ============================================================

(defparameter *module-table* nil)
(defparameter *bard-module* nil)
(defparameter *module* nil)

(defclass module ()
  ((interned-symbols :reader interned-symbols :initform (make-hash-table :test #'equal))
   (imported-symbols  :reader imported-symbols :initform (make-hash-table :test #'equal))
   (exported-symbols  :reader exported-symbols :initform (make-hash-table :test #'equal))
   (modules-used :reader modules-used :initform nil)
   (modules-using :reader modules-using :initform nil)))

(defun init-modules ()
  (setf *module-table* (make-hash-table :test #'equal))
  (setf *bard-module* (make-instance 'module))
  (setf (gethash "bard" *module-table*) *bard-module*)
  (setf *module* *bard-module*))

(defmethod intern ((s string)(m module))
  (setf (gethash s (interned-symbols m))
        (symbol s)))