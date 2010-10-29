;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          package.lisp
;;;; Project:       Bard HLVM
;;;; Purpose:       Bard pacakge definitions
;;;; Author:        mikel evins
;;;; Copyright:     2010 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(defpackage "BARD.READER"
  (:use :cl)
  (:nicknames "BR")
  (:shadow "READ")
  (:export "READ"))

(defpackage "BARD"
  (:use :cl)
  (:shadow)
  (:export))

