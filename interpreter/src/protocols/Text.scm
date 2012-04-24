;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          Text.scm
;;;; Project:       Bard
;;;; Purpose:       implementation of the Text protocol
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(include "~~lib/_gambit#.scm")
(##include "../values/type-macros.scm")

;;; ---------------------------------------------------------------------
;;; The Protocol
;;; ---------------------------------------------------------------------

(%define-protocol Text)

;;; text?
;;; ---------------------------------------------------------------------

(define bard:text? string?)

