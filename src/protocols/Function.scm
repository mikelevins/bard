;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          Function.scm
;;;; Project:       Bard
;;;; Purpose:       implementation of the Function protocol
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(##include "../values/type-macros.scm")
(##include "../values/function-macros.scm")

;;; ---------------------------------------------------------------------
;;; The Protocol
;;; ---------------------------------------------------------------------

(%define-protocol Function)


;;; function?
;;; ---------------------------------------------------------------------

(define bard:function? (%make-function name: 'function?))

(%function-add-method! bard:function? `(,Anything) (%method (x) false))
(%function-add-method! bard:function? `(,<function>) (%method (x) true))
