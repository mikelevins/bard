;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          function.scm
;;;; Project:       Bard VM
;;;; Purpose:       functions and methods
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(define-type function
  id: 1C9883E1-FEAC-4EEA-960F-5056AC363A01
  constructor: %private-make-function
  code
  debug-name)

(define (make-function code #!key (debug-name #f))
  (%private-make-function code debug-name))
