;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          characters.scm
;;;; Project:       bard
;;;; Purpose:       Bard character values
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

;;; ---------------------------------------------------------------------
;;; characters
;;; ---------------------------------------------------------------------

(define (bard:character x)
  (or (and (char? x) x)
      (error "Invalid value for character" x)))

(define (bard:character? x)
  (char? x))
