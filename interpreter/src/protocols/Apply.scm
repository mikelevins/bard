;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          Apply.scm
;;;; Project:       Bard
;;;; Purpose:       generic equality 
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(##include "../values/type-macros.scm")

;;; ---------------------------------------------------------------------
;;; The Protocol
;;; ---------------------------------------------------------------------

(%define-protocol Apply)

;;; applicable?
;;; ---------------------------------------------------------------------

(define bard:applicable? (%make-function name: 'applicable?))
(%function-add-method! bard:applicable? `(,Anything)(lambda (x y)(bard:false)))
(%function-add-method! bard:applicable? `(,<primitive-procedure>)(lambda (x y)(bard:true)))
(%function-add-method! bard:applicable? `(,<function>)(lambda (x y)(bard:true)))
(%function-add-method! bard:applicable? `(,<method>)(lambda (x y)(bard:true)))
(%function-add-method! bard:applicable? `(,<null>)(lambda (x y)(bard:true)))
(%function-add-method! bard:applicable? `(,<cons>)(lambda (x y)(bard:true)))
(%function-add-method! bard:applicable? `(,<string>)(lambda (x y)(bard:true)))
(%function-add-method! bard:applicable? `(,<frame>)(lambda (x y)(bard:true)))

