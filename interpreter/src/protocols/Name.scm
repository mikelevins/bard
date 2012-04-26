;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          Name.scm
;;;; Project:       Bard
;;;; Purpose:       implementation of the Name protocol
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(##include "../values/type-macros.scm")

;;; ---------------------------------------------------------------------
;;; The Protocol
;;; ---------------------------------------------------------------------

(%define-protocol Name)

;;; name?
;;; ---------------------------------------------------------------------

(define bard:name? (%make-function name: 'name?))

(%function-add-method! bard:name? `(,Anything) (lambda (x)(%false)))
(%function-add-method! bard:name? `(,<keyword>) (lambda (x)(%true)))
(%function-add-method! bard:name? `(,<symbol>) (lambda (x)(%true)))
