;;;; ***********************************************************************
;;;;
;;;; Name:          error.scm
;;;; Project:       Bard
;;;; Purpose:       error-handling utilities
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(define (not-yet-implemented fname #!optional (message ""))
  (error (string-append (->string fname) " is not yet implemented")))
