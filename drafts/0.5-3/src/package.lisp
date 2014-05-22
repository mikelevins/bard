;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          package.lisp
;;;; Project:       Bard
;;;; Purpose:       Bard package definition
;;;; Author:        mikel evins
;;;; Copyright:     2014 mikel evins
;;;;
;;;; ***********************************************************************

(defpackage #:bard
  (:use #:cl)
  (:shadow #:compile #:debug #:function #:make-method #:method #:optimize #:symbol))
