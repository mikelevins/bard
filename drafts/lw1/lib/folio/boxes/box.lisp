;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          box.lisp
;;;; Project:       folio - the Bard runtime
;;;; Purpose:       mutable containers for use with funtional collections
;;;; Author:        mikel evins
;;;; Copyright:     2010 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(defpackage "FOLIO.BOXES"
  (:use :cl)
  (:nicknames "BOX")
  (:shadow "GET")
  (:export "BOX" "GET" "MAKE" "PUT"))

(in-package :BOX)

;;; =====================================================================
;;; BOX API
;;; =====================================================================

(defclass box ()
  ((val :reader get :writer put :initform nil :initarg :value)))

(defun make (&optional val)
  (make-instance 'box :value val))