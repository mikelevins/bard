;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          Ratio.scm
;;;; Project:       Bard
;;;; Purpose:       implementation of the Ratio protocol
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(##include "../values/type-macros.scm")
(##include "../values/function-macros.scm")

;;; ---------------------------------------------------------------------
;;; The Protocol
;;; ---------------------------------------------------------------------

(%define-protocol Ratio)

;;; ratio?
;;; ---------------------------------------------------------------------

(define bard:ratio? (%make-function name: 'ratio?))

(%function-add-method! bard:ratio? `(,Anything) (%method (x) false))
(%function-add-method! bard:ratio? `(,<ratnum>) (%method (x) true))
