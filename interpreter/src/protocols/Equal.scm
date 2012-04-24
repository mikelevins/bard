;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          Equal.scm
;;;; Project:       Bard
;;;; Purpose:       generic equality 
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(include "~~lib/_gambit#.scm")
(##include "../values/type-macros.scm")

;;; ---------------------------------------------------------------------
;;; The Protocol
;;; ---------------------------------------------------------------------

(%define-protocol Equal)

;;; =
;;; ---------------------------------------------------------------------

(define bard:= (%make-function name: '=))
(%function-add-method! bard:= `(,Anything ,Anything)(lambda (x y)(equal? x y)))

