;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          Keyword.scm
;;;; Project:       Bard
;;;; Purpose:       implementation of the Keyword protocol
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(##include "../values/type-macros.scm")
(##include "../values/function-macros.scm")

;;; ---------------------------------------------------------------------
;;; The Protocol
;;; ---------------------------------------------------------------------

(%define-protocol Keyword)

;;; keyword?
;;; ---------------------------------------------------------------------

(define bard:keyword? (%make-function name: 'keyword?))

(%function-add-method! bard:keyword? `(,Anything) (%method (x) false))
(%function-add-method! bard:keyword? `(,<keyword>) (%method (x) true))

