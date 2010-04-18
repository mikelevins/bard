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
;;; Package bard-internal
;;; ============================================================

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defpackage |bard-internal|
    (:use common-lisp ccl)
    (:shadow = apply boolean compile count false find-symbol
             first fourth function intern map method
             read rest second sequence third true text
             unbound-variable)
    (:export #:= #:apply #:boolean #:boolean? #:character?  #:compile
    #:element #:false #:false? #:find-symbol #:get-key #:keyword?
    #:left #:map #:map? #:number? #:nothing #:prepend #:read #:right
    #:sequence #:sequence? #:symbol? #:text #:text? #:true #:true?)))

;;; ============================================================
;;; Package bard
;;; ============================================================

;;; Symbols defined in the bard package are in the bard module in bard.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defpackage |bard|
    (:use |bard-internal|)))
