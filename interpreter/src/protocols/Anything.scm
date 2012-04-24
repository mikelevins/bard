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

(include "~~lib/_gambit#.scm")
(##include "../values/type-macros.scm")

;;; ---------------------------------------------------------------------
;;; The Protocol
;;; ---------------------------------------------------------------------

(%define-protocol Anything)

;;; something?
;;; ---------------------------------------------------------------------

(define bard:something? (%make-function name: 'something?))

(%function-add-method! bard:something? `(,Anything) (lambda (x)(bard:true)))
(%function-add-method! bard:something? `(,<null>) (lambda (x)(bard:false)))

