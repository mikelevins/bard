;;;; ***********************************************************************
;;;;
;;;; Name:          error.scm
;;;; Project:       Bard
;;;; Purpose:       error conditions and messages
;;;; Author:        mikel evins
;;;; Copyright:     2016 by mikel evins
;;;;
;;;; ***********************************************************************

(declare (extended-bindings))

(define (%not-yet-implemented name)
  (error "not yet implemented: " name))
