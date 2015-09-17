;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          protocol-pairing.scm
;;;; Project:       Bard
;;;; Purpose:       arranging values in pairs
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(declare (extended-bindings))
(##include "type-signature-macros.scm")
(##include "protocol-macros.scm")

(define-protocol-function Pairing left
  signatures: (list (signature (Pair) #f (Anything))))

(define-primitive-method left (<pair>)
  car)

(define-protocol-function Pairing right
  signatures: (list (signature (Pair) #f (Anything))))

(define-primitive-method right (<pair>)
  cdr)

