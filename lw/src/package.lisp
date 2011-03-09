;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          package.asd
;;;; Project:       Bard
;;;; Purpose:       package definitions
;;;; Author:        mikel evins
;;;; Copyright:     2011 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(defpackage "BARD"
  (:use :cl :graylex :parsergen)
  (:shadow #:boolean #:character #:eval #:float #:function #:integer #:map #:method #:number #:sequence)
  (:export  #:print-usage #:run-batch #:run-repl))


