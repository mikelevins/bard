;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          type-signature-macros.scm
;;;; Project:       Bard
;;;; Purpose:       
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

(define-macro (signature input-types restarg output-types)
  `(make-signature (list ,@input-types) ,restarg (list ,@output-types)))
