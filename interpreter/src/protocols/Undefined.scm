;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          Undefined.scm
;;;; Project:       Bard
;;;; Purpose:       implementation of the Undefined protocol
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(##include "../values/type-macros.scm")

;;; ---------------------------------------------------------------------
;;; The Protocol
;;; ---------------------------------------------------------------------

(%define-protocol Undefined)

;;; undefined?
;;; ---------------------------------------------------------------------

(define bard:undefined? (%make-function name: 'undefined?))

(%function-add-method! bard:undefined? `(,Anything) (lambda (x)(%false)))
(%function-add-method! bard:undefined? `(,<undefined>) (lambda (x)(%true)))
