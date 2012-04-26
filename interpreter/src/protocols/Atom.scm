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

;;; ---------------------------------------------------------------------
;;; The Protocol
;;; ---------------------------------------------------------------------

(%define-protocol Atom)

;;; atom?
;;; ---------------------------------------------------------------------

(define bard:atom? (%make-function name: 'atom?))

(%function-add-method! bard:atom? `(,Anything) (lambda (x)(%true)))
(%function-add-method! bard:atom? `(,<cons>) (lambda (x)(%false)))
(%function-add-method! bard:atom? `(,<string>) (lambda (x)(%false)))
(%function-add-method! bard:atom? `(,<frame>) (lambda (x)(%false)))
