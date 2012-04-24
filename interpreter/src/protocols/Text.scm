;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          Text.scm
;;;; Project:       Bard
;;;; Purpose:       implementation of the Text protocol
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(##include "../values/type-macros.scm")

;;; ---------------------------------------------------------------------
;;; The Protocol
;;; ---------------------------------------------------------------------

(%define-protocol Text)

;;; text?
;;; ---------------------------------------------------------------------

(define bard:text? (%make-function name: 'text?))

(%function-add-method! bard:text? `(,Anything) (lambda (x)(bard:false)))
(%function-add-method! bard:text? `(,<string>) (lambda (x)(bard:true)))

