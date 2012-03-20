;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          text.scm
;;;; Project:       Bard
;;;; Purpose:       text values
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

;;;---------------------------------------------------------------------
;;; API
;;;---------------------------------------------------------------------

(define bard:text? string?)

(define (bard:make-text characters)
  (list->string characters))

(define (text . characters)
  (bard:make-text characters))