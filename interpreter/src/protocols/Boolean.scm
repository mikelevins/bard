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

;;; ---------------------------------------------------------------------
;;; The Protocol
;;; ---------------------------------------------------------------------

(%define-protocol Boolean)

;;; boolean?
;;; ---------------------------------------------------------------------

(define bard:boolean? (%make-function name: 'boolean?))

(%function-add-method! bard:boolean? `(,Anything) (lambda (x)(%false)))
(%function-add-method! bard:boolean? `(,<boolean>) (lambda (x)(%true)))

;;; false?
;;; ---------------------------------------------------------------------

(define bard:false? (%make-function name: 'false?))

(%function-add-method! bard:false? `(,Anything) (lambda (x)(%false)))
(%function-add-method! bard:false? `(,<undefined>) (lambda (x)(%false)))
(%function-add-method! bard:false? `(,<null>) (lambda (x)(%true)))
(%function-add-method! bard:false? `(,<boolean>) (lambda (x) (not x)))

;;; true?
;;; ---------------------------------------------------------------------

(define bard:true? (%make-function name: 'true?))

(%function-add-method! bard:true? `(,Anything) (lambda (x)(%true)))
(%function-add-method! bard:true? `(,<undefined>) (lambda (x)(%false)))
(%function-add-method! bard:true? `(,<null>) (lambda (x)(%false)))
(%function-add-method! bard:true? `(,<boolean>) (lambda (x) x))

