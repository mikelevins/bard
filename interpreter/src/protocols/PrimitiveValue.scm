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

;;; ---------------------------------------------------------------------
;;; The Protocol
;;; ---------------------------------------------------------------------

(%define-protocol PrimitiveValue)

;;; primitive-value?
;;; ---------------------------------------------------------------------

(define bard:primitive-value? (%make-function name: 'primitive-value?))

(%function-add-method! bard:primitive-value? `(,Anything) (lambda (x)(%false)))
(%function-add-method! bard:primitive-value? `(,<undefined>) (lambda (x)(%true)))
(%function-add-method! bard:primitive-value? `(,<null>) (lambda (x)(%true)))
(%function-add-method! bard:primitive-value? `(,<character>) (lambda (x)(%true)))
(%function-add-method! bard:primitive-value? `(,<boolean>) (lambda (x)(%true)))
(%function-add-method! bard:primitive-value? `(,<symbol>) (lambda (x)(%true))) 
(%function-add-method! bard:primitive-value? `(,<keyword>) (lambda (x)(%true)))
(%function-add-method! bard:primitive-value? `(,<flonum>) (lambda (x)(%true)))
(%function-add-method! bard:primitive-value? `(,<ratnum>) (lambda (x)(%true)))
(%function-add-method! bard:primitive-value? `(,<fixnum>) (lambda (x)(%true)))
(%function-add-method! bard:primitive-value? `(,<bignum>) (lambda (x)(%true)))
(%function-add-method! bard:primitive-value? `(,<primitive-procedure>) (lambda (x)(%true)))
(%function-add-method! bard:primitive-value? `(,<cons>) (lambda (x)(%true)))
(%function-add-method! bard:primitive-value? `(,<string>) (lambda (x)(%true)))
