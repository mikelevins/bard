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

(%function-add-method! bard:primitive-value? `(,Anything) (lambda (x)(bard:false)))
(%function-add-method! bard:primitive-value? `(,<undefined>) (lambda (x)(bard:true)))
(%function-add-method! bard:primitive-value? `(,<null>) (lambda (x)(bard:true)))
(%function-add-method! bard:primitive-value? `(,<character>) (lambda (x)(bard:true)))
(%function-add-method! bard:primitive-value? `(,<boolean>) (lambda (x)(bard:true)))
(%function-add-method! bard:primitive-value? `(,<symbol>) (lambda (x)(bard:true))) 
(%function-add-method! bard:primitive-value? `(,<keyword>) (lambda (x)(bard:true)))
(%function-add-method! bard:primitive-value? `(,<flonum>) (lambda (x)(bard:true)))
(%function-add-method! bard:primitive-value? `(,<ratnum>) (lambda (x)(bard:true)))
(%function-add-method! bard:primitive-value? `(,<fixnum>) (lambda (x)(bard:true)))
(%function-add-method! bard:primitive-value? `(,<bignum>) (lambda (x)(bard:true)))
(%function-add-method! bard:primitive-value? `(,<primitive-procedure>) (lambda (x)(bard:true)))
(%function-add-method! bard:primitive-value? `(,<cons>) (lambda (x)(bard:true)))
(%function-add-method! bard:primitive-value? `(,<string>) (lambda (x)(bard:true)))
