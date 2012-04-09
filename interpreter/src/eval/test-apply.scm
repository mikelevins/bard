;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          test-apply.scm
;;;; Project:       Bard
;;;; Purpose:       tests for apply
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(include "~~lib/_gambit#.scm")
(##include "../values/function-macros.scm")

(%define-function (apply-test1))
(%function-add-method! apply-test1 `(,Anything ,Anything) 
                       (%make-method name: 'apply-test1 params: '(x y) 
                                     body: `(,+ x y)))

(show (%apply apply-test1 '()))
(show (%apply apply-test1 '(2 3)))
(show (%eval 'x (%add-binding (%null-environment) 'x 2)))
(show (%apply apply-test1 '(x 3) (%add-binding (%null-environment) 'x 2)))
