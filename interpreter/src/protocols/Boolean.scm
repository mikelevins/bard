;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          Boolean.scm
;;;; Project:       Bard
;;;; Purpose:       implementation of the Boolean protocol
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(##include "../values/type-macros.scm")
(##include "../values/function-macros.scm")

;;; ---------------------------------------------------------------------
;;; The Protocol
;;; ---------------------------------------------------------------------

(%define-protocol Boolean)

;;; boolean?
;;; ---------------------------------------------------------------------

(define bard:boolean? (%make-function name: 'boolean?))

(%function-add-method! bard:boolean? `(,Anything) (%method (_) false))
(%function-add-method! bard:boolean? `(,<boolean>) (%method (_) true))

;;; false?
;;; ---------------------------------------------------------------------

(define bard:false? (%make-function name: 'false?))

(%function-add-method! bard:false? `(,Anything) (%method (_) false))
(%function-add-method! bard:false? `(,<undefined>) (%method (_) false))
(%function-add-method! bard:false? `(,<null>) (%method (_) true))
(%function-add-method! bard:false? `(,<boolean>) (%method (x) (not x)))

;;; true?
;;; ---------------------------------------------------------------------

(define bard:true? (%make-function name: 'true?))

(%function-add-method! bard:true? `(,Anything) (%method (_) true))
(%function-add-method! bard:true? `(,<undefined>) (%method (_) false))
(%function-add-method! bard:true? `(,<null>) (%method (_) false))
(%function-add-method! bard:true? `(,<boolean>) (%method (x) x))

