;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          Float.scm
;;;; Project:       Bard
;;;; Purpose:       implementation of the Float protocol
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(##include "../values/type-macros.scm")
(##include "../values/function-macros.scm")

;;; ---------------------------------------------------------------------
;;; The Protocol
;;; ---------------------------------------------------------------------

(%define-protocol Float)

;;; float?
;;; ---------------------------------------------------------------------

(define bard:float? (%make-function name: 'float?))

(%function-add-method! bard:float? `(,Anything) (%method (x) false))
(%function-add-method! bard:float? `(,<flonum>) (%method (x) true))
