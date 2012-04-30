;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          Anything.scm
;;;; Project:       Bard
;;;; Purpose:       implementation of the Anything protocol
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(##include "../values/type-macros.scm")
(##include "../values/function-macros.scm")

;;; ---------------------------------------------------------------------
;;; The Protocol
;;; ---------------------------------------------------------------------

(%define-protocol Anything)

;;; something?
;;; ---------------------------------------------------------------------

(define bard:something? (%make-function name: 'something?))

(%function-add-method! bard:something? `(,Anything)(%method (_) true))
(%function-add-method! bard:something? `(,<null>)(%method (_) false))
