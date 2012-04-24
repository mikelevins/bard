;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          Character.scm
;;;; Project:       Bard
;;;; Purpose:       implementation of the Character protocol
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(##include "../values/type-macros.scm")

;;; ---------------------------------------------------------------------
;;; The Protocol
;;; ---------------------------------------------------------------------

(%define-protocol Character)

;;; character?
;;; ---------------------------------------------------------------------

(define bard:character? (%make-function name: 'character?))

(%function-add-method! bard:character? `(,Anything) (lambda (x)(bard:false)))
(%function-add-method! bard:character? `(,<character>) (lambda (x)(bard:true)))

