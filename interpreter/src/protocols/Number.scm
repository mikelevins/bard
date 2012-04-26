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

;;; ---------------------------------------------------------------------
;;; The Protocol
;;; ---------------------------------------------------------------------

(%define-protocol Number)

;;; number?
;;; ---------------------------------------------------------------------

(define bard:number? (%make-function name: 'number?))

(%function-add-method! bard:number? `(,Anything) (lambda (x)(%false)))
(%function-add-method! bard:number? `(,<fixnum>) (lambda (x)(%true)))
(%function-add-method! bard:number? `(,<bignum>) (lambda (x)(%true)))
(%function-add-method! bard:number? `(,<flonum>) (lambda (x)(%true)))
(%function-add-method! bard:number? `(,<ratnum>) (lambda (x)(%true)))

