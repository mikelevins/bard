;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          package.lisp
;;;; Project:       Bard
;;;; Purpose:       package definitions
;;;; Author:        mikel evins
;;;; Copyright:     2010 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(defpackage "BARD"
  (:use :cl :folio.as)
  (:shadow #:= #:character #:float #:integer #:map #:sequence)
  (:import-from :folio.fn #:$ #:^))


(defpackage "SMUG"
  (:use :cl))


(defpackage "BARD-READER"
  (:use :cl :parsergen)
  (:shadow #:read))

