;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          packages.lisp
;;;; Project:       Bard
;;;; Purpose:       bard system packages
;;;; Author:        mikel evins
;;;; Copyright:     2010 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(defpackage "BARD"
  (:use :cl)
  (:shadow "READ" "READ-TABLE")
  (:export "READ" "READ-TABLE"))


