;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          cell.lisp
;;;; Project:       Bard
;;;; Purpose:       mutable containers for use with funtional collections
;;;; Author:        mikel evins
;;;; Copyright:     2010 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(defpackage "BARD.CELLS"
  (:use :cl :fn)
  (:nicknames "CEL")
  (:shadow "GET")
  (:export "GET" "MAKE" "PUT"))

(in-package :CEL)

;;; =====================================================================
;;; CELL API
;;; =====================================================================

(defclass cell ()
  ((val :reader get :writer put :initform nil :initarg :value)))

(defun make (&optional val)
  (make-instance 'cell :value val))