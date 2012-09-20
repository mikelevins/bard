;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          applicable.scm
;;;; Project:       Bard
;;;; Purpose:       Bard applicable objects 
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(define-type applicable
  extender: define-applicable)

(define-applicable function
  parameters
  return-spec
  method-table)

(define-applicable method
  parameters
  code
  environment)

(define-applicable primitive
  name
  required-count
  restarg?
  function)
