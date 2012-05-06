;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          Atom.scm
;;;; Project:       Bard
;;;; Purpose:       implementation of the Atom protocol
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(##include "../values/type-macros.scm")
(##include "../values/function-macros.scm")

;;; ---------------------------------------------------------------------
;;; The Protocol
;;; ---------------------------------------------------------------------

(%define-protocol Atom)

;;; atom?
;;; ---------------------------------------------------------------------

(define bard:atom? (%make-function name: 'atom?))

(%function-add-method! bard:atom? `(,Anything) (%method (_) true))
(%function-add-method! bard:atom? `(,<cons>) (%method (_) false))
(%function-add-method! bard:atom? `(,<string>) (%method (_) false))
(%function-add-method! bard:atom? `(,<frame>) (%method (_) false))
