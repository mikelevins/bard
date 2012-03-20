;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          nothing.scm
;;;; Project:       Bard
;;;; Purpose:       the null value
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

;;;---------------------------------------------------------------------
;;; API
;;;---------------------------------------------------------------------

(define bard:nothing '())
(define bard:nothing? null?)
(define (bard:something? x)(not (bard:nothing? x)))

