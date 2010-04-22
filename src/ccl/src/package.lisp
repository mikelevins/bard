;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          package.lisp
;;;; Project:       Bard - a modern Lisp
;;;; Purpose:       package definitions
;;;; Author:        mikel evins
;;;; Copyright:     2009 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

;;; ============================================================
;;; Package bard-internal
;;; ============================================================

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defpackage |bard-internal|
    (:use common-lisp ccl)
    (:shadow #:= #:boolean #:false #:read #:true)
    (:export #:= #:boolean #:false #:read #:true)))

;;; ============================================================
;;; Package bard
;;; ============================================================

;;; Symbols defined in the bard package are in the bard module in bard.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defpackage |bard|
    (:use |bard-internal|)))

