;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          package.lisp
;;;; Project:       bard
;;;; Purpose:       package definition
;;;; Author:        mikel evins
;;;; Requirements:  Clozure Common Lisp
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

;;; ----------------------------------------------------------------------
;;; package
;;; ----------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defpackage BARD 
    (:use common-lisp ccl)
    (:shadow atom compile count first read rest type type-of)))

