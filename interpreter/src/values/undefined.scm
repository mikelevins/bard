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

(define (%undefined) #!unbound)
(define (%undefined? x) (eqv? x #!unbound))
(define (%defined? x)(not (%undefined? x)))

