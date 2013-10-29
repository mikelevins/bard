;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          globals.lisp
;;;; Project:       Bard
;;;; Purpose:       simple implementation of global variables 
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard)

;;; ---------------------------------------------------------------------
;;; bard globals
;;; ---------------------------------------------------------------------

(defparameter *bard-globals* (make-hash-table))

(defmethod get-global ((var symbol))
  (gethash var *bard-globals* *undefined*))

(defmethod set-global! ((var symbol) val)
  (setf (gethash var *bard-globals*) val))

