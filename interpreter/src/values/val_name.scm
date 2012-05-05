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

(define %symbol? symbol?)
(define %keyword? keyword?)
(define (%name? x) 
  (or (%symbol? x)
      (%keyword? x)))

