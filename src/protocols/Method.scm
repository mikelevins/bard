;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          Method.scm
;;;; Project:       Bard
;;;; Purpose:       implementation of the Method protocol
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(##include "../values/type-macros.scm")
(##include "../values/function-macros.scm")

;;; ---------------------------------------------------------------------
;;; The Protocol
;;; ---------------------------------------------------------------------

(%define-protocol Method)


;;; method?
;;; ---------------------------------------------------------------------

(define bard:method? (%make-function name: 'method?))

(%function-add-method! bard:method? `(,Anything) (%method (x) false))
(%function-add-method! bard:method? `(,<method>) (%method (x) true))