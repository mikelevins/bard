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

(define-macro (%define-primitive-type name tag)
  `(begin
     (define ,name (%make-primitive-type ',name ,tag))
     (table-set! $bard-primitive-type-table ,tag ,name)
     ,name))

(define-macro (%define-structure-type name gambit-structure-type predicate)
  `(begin
     (define ,name (%make-structure-type ',name ,gambit-structure-type ,predicate))
     (table-set! $bard-structure-type-table ,gambit-structure-type ,name)
     ,name))

(define-macro (%define-protocol name)
  `(begin
     (define ,name (%make-protocol ',name))
     ',name))

