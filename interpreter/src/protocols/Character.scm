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
(##include "../values/function-macros.scm")

;;; ---------------------------------------------------------------------
;;; The Protocol
;;; ---------------------------------------------------------------------

(%define-protocol Character)

;;; character?
;;; ---------------------------------------------------------------------

(define bard:character? (%make-function name: 'character?))

(%function-add-method! bard:character? `(,Anything) (%method (_) false))
(%function-add-method! bard:character? `(,<character>) (%method (_) true))

