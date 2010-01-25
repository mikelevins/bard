;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: asdf -*-
;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          package.lisp
;;;; Project:       Bard - a near-minimal Cocoa application
;;;; Purpose:       define the Bard package
;;;; Author:        mikel evins
;;;; Copyright:     2009 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

;;; ------------------------------------------------------------
;;; Package UTILITIES
;;; ------------------------------------------------------------
;;; Symbols for the use of users

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defpackage UTILITIES 
    (:nicknames :util)
    (:use common-lisp ccl)))

;;; ------------------------------------------------------------
;;; Package BARD-INTERNALS
;;; ------------------------------------------------------------
;;; Private, internal symbols

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defpackage BARD-INTERNAL
    (:nicknames :bint)
    (:use common-lisp ccl)
    (:shadow character false first fourth intern map number read rest second symbol third true)))

;;; ------------------------------------------------------------
;;; Package BARD
;;; ------------------------------------------------------------
;;; Symbols for the use of users

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defpackage BARD 
    (:use common-lisp ccl bint)
    (:shadow compile first fourth intern map read rest second sequence third)))

