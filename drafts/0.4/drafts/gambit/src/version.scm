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


(define $bardo-version (vector 0 4 0))

(define (bardo-major-version)
  (vector-ref $bardo-version 0))

(define (bardo-minor-version)
  (vector-ref $bardo-version 1))

(define (bardo-update-version)
  (vector-ref $bardo-version 2))

(define (bardo-version-string)
  (string-append (number->string (bardo-major-version))
                 "."
                 (number->string (bardo-minor-version))
                 "."
                 (number->string (bardo-update-version))))
