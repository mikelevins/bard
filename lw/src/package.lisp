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
  (:use :cl :split-sequence :graylex :parsergen)
  (:shadow #:apply #:boolean #:character #:eval #:first #:float #:function #:integer 
           #:length #:map #:method #:number #:rest #:sequence)
  (:export  #:print-usage #:run-batch #:run-repl))


