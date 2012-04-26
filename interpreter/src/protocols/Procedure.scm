;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          Procedure.scm
;;;; Project:       Bard
;;;; Purpose:       implementation of the Procedure protocol
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(##include "../values/type-macros.scm")

;;; ---------------------------------------------------------------------
;;; The Protocol
;;; ---------------------------------------------------------------------

(%define-protocol Procedure)

;;; procedure?
;;; ---------------------------------------------------------------------

(define bard:procedure? (%make-function name: 'procedure?))

(%function-add-method! bard:procedure? `(,Anything) (lambda (x)(%false)))
(%function-add-method! bard:procedure? `(,<primitive-procedure>) (lambda (x)(%true)))
(%function-add-method! bard:procedure? `(,<function>) (lambda (x)(%true)))
(%function-add-method! bard:procedure? `(,<method>) (lambda (x)(%true)))

