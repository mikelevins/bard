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

(define-macro (bard:define-primitive-type name tag)
  `(begin
     (define ,name (bard:%make-primitive-type ',name ,tag))
     (table-set! $bard-primitive-type-table ,tag ,name)))

(define-macro (bard:define-structure-type name predicate)
  `(begin
     (define ,name (%def-structure-type ',name ,predicate))
     ',name))

(define-macro (bard:define-category name)
  `(begin
     (define ,name (%def-category ',name))
     ',name))


