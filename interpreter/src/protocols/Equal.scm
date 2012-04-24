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

(##include "../values/type-macros.scm")

(define bard:= (%make-function name: '=))
(%function-add-method! bard:= `(,Anything ,Anything)(lambda (x y)(equal? x y)))

