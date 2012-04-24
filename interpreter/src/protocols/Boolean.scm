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

(%function-add-method! bard:boolean? `(,Anything) (lambda (x)(bard:false)))
(%function-add-method! bard:boolean? `(,<boolean>) (lambda (x)(bard:true)))

;;; false?
;;; ---------------------------------------------------------------------

(define bard:false? (%make-function name: 'false?))

(%function-add-method! bard:false? `(,Anything) (lambda (x)(bard:false)))
(%function-add-method! bard:false? `(,<undefined>) (lambda (x)(bard:false)))
(%function-add-method! bard:false? `(,<null>) (lambda (x)(bard:true)))
(%function-add-method! bard:false? `(,<boolean>) (lambda (x) (not x)))

;;; true?
;;; ---------------------------------------------------------------------

(define bard:true? (%make-function name: 'true?))

(%function-add-method! bard:true? `(,Anything) (lambda (x)(bard:true)))
(%function-add-method! bard:true? `(,<undefined>) (lambda (x)(bard:false)))
(%function-add-method! bard:true? `(,<null>) (lambda (x)(bard:false)))
(%function-add-method! bard:true? `(,<boolean>) (lambda (x) x))

