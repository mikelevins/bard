;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          Null.scm
;;;; Project:       Bard
;;;; Purpose:       implementation of the Null protocol
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(##include "../values/type-macros.scm")

;;; ---------------------------------------------------------------------
;;; The Protocol
;;; ---------------------------------------------------------------------

(%define-protocol Null)

;;; nothing?
;;; ---------------------------------------------------------------------

(define bard:nothing? (%make-function name: 'nothing?))

(%function-add-method! bard:nothing? `(,Anything) (lambda (x)(%false)))
(%function-add-method! bard:nothing? `(,<null>) (lambda (x)(%true)))
