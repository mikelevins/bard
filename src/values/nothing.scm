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

(define (%nothing) '())
(define %nothing? null?)
(define %null? null?)
(define (%something? x)(not (%nothing? x)))

