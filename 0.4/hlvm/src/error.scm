;;;; ***********************************************************************
;;;;
;;;; Name:          error.scm
;;;; Project:       Bard
;;;; Purpose:       error-handling utilities
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(declare (extended-bindings))

(define (not-yet-implemented fname #!optional (message ""))
  (error (string-append (object->string fname) " is not yet implemented")))
