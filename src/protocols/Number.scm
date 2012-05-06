;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          Number.scm
;;;; Project:       Bard
;;;; Purpose:       implementation of the Number protocol
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(##include "../values/type-macros.scm")
(##include "../values/function-macros.scm")

;;; ---------------------------------------------------------------------
;;; The Protocol
;;; ---------------------------------------------------------------------

(%define-protocol Number)

;;; number?
;;; ---------------------------------------------------------------------

(define bard:number? (%make-function name: 'number?))

(%function-add-method! bard:number? `(,Anything) (%method (x) false))
(%function-add-method! bard:number? `(,<fixnum>) (%method (x) true))
(%function-add-method! bard:number? `(,<bignum>) (%method (x) true))
(%function-add-method! bard:number? `(,<flonum>) (%method (x) true))
(%function-add-method! bard:number? `(,<ratnum>) (%method (x) true))

