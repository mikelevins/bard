;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          boolean.scm
;;;; Project:       Bard
;;;; Purpose:       boolean values
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

;;;---------------------------------------------------------------------
;;; API
;;;---------------------------------------------------------------------

(define (%false) #f)

(define (%false? x) 
  (or (eqv? x (%false))
      (%nothing? x)))

(define (%true) #t)
(define (%true? x) (not (%false? x)))

(define (%boolean? x)
  (or (eqv? x (%false))
      (eqv? x (%true))))

