;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          Float.scm
;;;; Project:       Bard
;;;; Purpose:       implementation of the Float protocol
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(##include "../values/type-macros.scm")

;;; ---------------------------------------------------------------------
;;; The Protocol
;;; ---------------------------------------------------------------------

(%define-protocol Float)

;;; float?
;;; ---------------------------------------------------------------------

(define bard:float? (%make-function name: 'float?))

(%function-add-method! bard:float? `(,Anything) (lambda (x)(bard:false)))
(%function-add-method! bard:float? `(,<flonum>) (lambda (x)(bard:true)))
