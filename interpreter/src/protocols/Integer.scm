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

(include "~~lib/_gambit#.scm")
(##include "../values/type-macros.scm")

;;; ---------------------------------------------------------------------
;;; The Protocol
;;; ---------------------------------------------------------------------

(%define-protocol Integer)

;;; integer?
;;; ---------------------------------------------------------------------

(define bard:integer? (%make-integer name: 'integer?))

(%integer-add-method! bard:integer? `(,Anything) (lambda (x)(bard:false)))
(%integer-add-method! bard:integer? `(,<fixnum>) (lambda (x)(bard:true)))
(%integer-add-method! bard:integer? `(,<bignum>) (lambda (x)(bard:true)))

