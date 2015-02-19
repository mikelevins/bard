;;;; ***********************************************************************
;;;;
;;;; Name:          functions.scm
;;;; Project:       Bard
;;;; Purpose:       representation of Bard functions
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(declare (extended-bindings))

(define-structure function
  input-types output-types methods)

(define (kernel:eval-function expr env)
  #f)
