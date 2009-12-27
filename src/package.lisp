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
;;; Package BARD-INTERNALS
;;; ------------------------------------------------------------
;;; Private, internal symbols

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defpackage BARD-INTERNAL
    (:nicknames :bint)
    (:use common-lisp ccl)
    (:shadow character false first map number read rest symbol true)))

;;; ------------------------------------------------------------
;;; Package BARD
;;; ------------------------------------------------------------
;;; Symbols for the use of users

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defpackage BARD 
    (:use common-lisp ccl bint)
    (:shadow read)))

