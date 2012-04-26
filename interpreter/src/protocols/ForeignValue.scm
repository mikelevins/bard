;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          ForeignValue.scm
;;;; Project:       Bard
;;;; Purpose:       implementation of the ForeignValue protocol
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(##include "../values/type-macros.scm")

;;; ---------------------------------------------------------------------
;;; The Protocol
;;; ---------------------------------------------------------------------

(%define-protocol ForeignValue)

;;; foreign-value?
;;; ---------------------------------------------------------------------

(define bard:foreign-value? (%make-function name: 'foreign-value?))

(%function-add-method! bard:foreign-value? `(,Anything) (lambda (x)(%false)))

