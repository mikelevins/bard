;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          Integer.scm
;;;; Project:       Bard
;;;; Purpose:       implementation of the Integer protocol
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(##include "../values/type-macros.scm")

;;; ---------------------------------------------------------------------
;;; The Protocol
;;; ---------------------------------------------------------------------

(%define-protocol Integer)

;;; integer?
;;; ---------------------------------------------------------------------

(define bard:integer? (%make-function name: 'integer?))

(%function-add-method! bard:integer? `(,Anything) (lambda (x)(%false)))
(%function-add-method! bard:integer? `(,<fixnum>) (lambda (x)(%true)))
(%function-add-method! bard:integer? `(,<bignum>) (lambda (x)(%true)))

