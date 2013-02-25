;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          banner.scm
;;;; Project:       Bard
;;;; Purpose:       startup banner
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

(define $banner-message "Bard VM")

(define (display-banner)
  (newline)
  (display $banner-message)
  (display " v")
  (display (bard-version-string))
  (newline))

(define (display-usage)
  (newline)
  (newline)
  (display "USAGE: bardvm bardo-file")
  (newline))



