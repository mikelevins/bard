;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          Equal.scm
;;;; Project:       Bard
;;;; Purpose:       equality comparisons
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(##include "../values/function-macros.scm")
(##include "../values/type-macros.scm")

(define bard:= (%make-function name: '=))
(%function-add-method! bard:= `(,Anything ,Anything)(lambda (x y)(equal? x y)))

