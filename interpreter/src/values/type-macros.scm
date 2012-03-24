;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          type-macros.scm
;;;; Project:       Bard
;;;; Purpose:       conveniences for use with types
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(define-macro (bard:define-type name tag)
  `(begin
     (define ,name (bard:%make-type ',name ,tag))
     (table-set! $bard-type-table ,tag ,name)))


