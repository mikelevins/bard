;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          version.scm
;;;; Project:       Bard
;;;; Purpose:       the Bard interpreter version string
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************


(define $bard-vm-version (vector 0 4 0))

(define (bard-major-version)
  (vector-ref $bard-vm-version 0))

(define (bard-minor-version)
  (vector-ref $bard-vm-version 1))

(define (bard-update-version)
  (vector-ref $bard-vm-version 2))

(define (bard-version-string)
  (string-append (number->string (bard-major-version))
                 "."
                 (number->string (bard-minor-version))
                 "."
                 (number->string (bard-update-version))))
