;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: asdf -*-
;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          package.lisp
;;;; Project:       Bard - a modern Lisp
;;;; Purpose:       package defintions
;;;; Author:        mikel evins
;;;; Copyright:     2009 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

;;; ============================================================
;;; Package BARD
;;; ============================================================

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defpackage BARD 
    (:use common-lisp ccl)
    (:shadow = boolean character compile false find-symbol first fourth intern
             keyword map number read rest second sequence 
             symbol third true text unbound-variable)
    (:export #:= #:boolean #:boolean? #:character #:character?
    #:compile #:element #:false #:false? #:find-symbol #:get-key
    #:keyword #:keyword? #:left #:map #:map? #:number #:number? #:pair
    #:pair? #:prepend #:read #:right #:sequence
    #:sequence? #:symbol #:symbol? #:text #:text? #:true #:true?
    #:void)))
