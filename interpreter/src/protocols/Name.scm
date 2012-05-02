;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          Name.scm
;;;; Project:       Bard
;;;; Purpose:       implementation of the Name protocol
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(##include "../values/type-macros.scm")
(##include "../values/function-macros.scm")

;;; ---------------------------------------------------------------------
;;; The Protocol
;;; ---------------------------------------------------------------------

(%define-protocol Name)

;;; name?
;;; ---------------------------------------------------------------------

(define bard:name? (%make-function name: 'name?))

(%function-add-method! bard:name? `(,Anything) (%method (x) false))
(%function-add-method! bard:name? `(,<keyword>) (%method (x) true))
(%function-add-method! bard:name? `(,<symbol>) (%method (x) true))
