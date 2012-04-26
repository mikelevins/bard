;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          Type.scm
;;;; Project:       Bard
;;;; Purpose:       implementation of the Type protocol
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(##include "../values/type-macros.scm")

;;; ---------------------------------------------------------------------
;;; The Protocol
;;; ---------------------------------------------------------------------

(%define-protocol Type)

;;; type
;;; ---------------------------------------------------------------------

(define bard:type (%make-function name: 'type))

(define %bard-type %object->bard-type)

(%function-add-method! bard:type `(,Anything) %bard-type)

;;; type?
;;; ---------------------------------------------------------------------

(define bard:type? (%make-function name: 'type?))

(define %bard-type? %type?)

(%function-add-method! bard:type? `(,Anything) %bard-type?)
