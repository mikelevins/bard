;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          name.scm
;;;; Project:       Bard
;;;; Purpose:       name types
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

;;;---------------------------------------------------------------------
;;; API
;;;---------------------------------------------------------------------

(define bard:symbol? symbol?)
(define bard:keyword? keyword?)
(define (bard:name? x) 
  (or (bard:symbol? x)
      (bard:keyword? x)))

