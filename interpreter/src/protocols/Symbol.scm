;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          Symbol.scm
;;;; Project:       Bard
;;;; Purpose:       implementation of the Symbol protocol
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(##include "../values/type-macros.scm")

;;; ---------------------------------------------------------------------
;;; The Protocol
;;; ---------------------------------------------------------------------

(%define-protocol Symbol)

;;; symbol?
;;; ---------------------------------------------------------------------

(define bard:symbol? (%make-function name: 'symbol?))

(%function-add-method! bard:symbol? `(,Anything) (lambda (x)(bard:false)))
(%function-add-method! bard:symbol? `(,<symbol>) (lambda (x)(bard:true)))

