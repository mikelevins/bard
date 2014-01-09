;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          singletons.lisp
;;;; Project:       Bard
;;;; Purpose:       representation of Bard singletons
;;;; Author:        mikel evins
;;;; Copyright:     2014 mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard)

;;; ---------------------------------------------------------------------
;;; singleton representation
;;; ---------------------------------------------------------------------

(defclass bard-singleton ()
  ((value :accessor %value :initarg :value)))

(defmethod print-object ((obj bard-singleton)(s stream))
  (princ "#[<singleton>] { value: ")
  (princ (%value obj) s)
  (princ " }"))
