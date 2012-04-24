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

(include "~~lib/_gambit#.scm")
(##include "../values/type-macros.scm")

;;; ---------------------------------------------------------------------
;;; The Protocol
;;; ---------------------------------------------------------------------

(%define-protocol Anything)

;;; atom?
;;; ---------------------------------------------------------------------

(define bard:atom? (%make-function name: 'atom?))

(%function-add-method! bard:atom? `(,Anything) (lambda (x)(bard:true)))
(%function-add-method! bard:atom? `(,<cons>) (lambda (x)(bard:false)))
(%function-add-method! bard:atom? `(,<string>) (lambda (x)(bard:false)))
(%function-add-method! bard:atom? `(,<frame>) (lambda (x)(bard:false)))
