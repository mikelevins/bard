;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          modules.lisp
;;;; Project:       Bard
;;;; Purpose:       model Bard modules as Common Lisp pakages
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:bard)

(defpackage "bard.keyword")
(defpackage "bard.core")
(defpackage "bard.user")

(defparameter *module* (find-package "bard.user"))