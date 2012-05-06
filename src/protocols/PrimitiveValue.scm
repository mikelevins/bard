;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          PrimitiveValue.scm
;;;; Project:       Bard
;;;; Purpose:       implementation of the PrimitiveValue protocol
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(##include "../values/type-macros.scm")
(##include "../values/function-macros.scm")

;;; ---------------------------------------------------------------------
;;; The Protocol
;;; ---------------------------------------------------------------------

(%define-protocol PrimitiveValue)

;;; primitive-value?
;;; ---------------------------------------------------------------------

(define bard:primitive-value? (%make-function name: 'primitive-value?))

(%function-add-method! bard:primitive-value? `(,Anything) (%method (x) false))
(%function-add-method! bard:primitive-value? `(,<undefined>) (%method (x) true))
(%function-add-method! bard:primitive-value? `(,<null>) (%method (x) true))
(%function-add-method! bard:primitive-value? `(,<character>) (%method (x) true))
(%function-add-method! bard:primitive-value? `(,<boolean>) (%method (x) true))
(%function-add-method! bard:primitive-value? `(,<symbol>) (%method (x) true)) 
(%function-add-method! bard:primitive-value? `(,<keyword>) (%method (x) true))
(%function-add-method! bard:primitive-value? `(,<flonum>) (%method (x) true))
(%function-add-method! bard:primitive-value? `(,<ratnum>) (%method (x) true))
(%function-add-method! bard:primitive-value? `(,<fixnum>) (%method (x) true))
(%function-add-method! bard:primitive-value? `(,<bignum>) (%method (x) true))
(%function-add-method! bard:primitive-value? `(,<primitive-procedure>) (%method (x) true))
(%function-add-method! bard:primitive-value? `(,<cons>) (%method (x) true))
(%function-add-method! bard:primitive-value? `(,<string>) (%method (x) true))
