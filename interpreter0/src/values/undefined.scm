;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          undefined.scm
;;;; Project:       Bard
;;;; Purpose:       the undefined value
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

;;;---------------------------------------------------------------------
;;; API
;;;---------------------------------------------------------------------

(define (bard:undefined) #!unbound)
(define (bard:undefined? x) (eqv? x #!unbound))
(define (bard:defined? x)(not (bard:undefined? x)))

