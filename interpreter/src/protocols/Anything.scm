;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          Anything.scm
;;;; Project:       Bard
;;;; Purpose:       implementation of the Anything protocol
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(##include "../values/type-macros.scm")

;;; ---------------------------------------------------------------------
;;; The Protocol
;;; ---------------------------------------------------------------------

(%define-protocol Anything)

;;; something?
;;; ---------------------------------------------------------------------

(define bard:something? (%make-function name: 'something?))

(%function-add-method! bard:something? `(,Anything) (lambda (x)(%true)))
(%function-add-method! bard:something? `(,<null>) (lambda (x)(%false)))

