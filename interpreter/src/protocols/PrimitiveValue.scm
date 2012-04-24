;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          PrimitiveValue.scm
;;;; Project:       Bard
;;;; Purpose:       implementation of the PrimitiveValue protocol
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(include "~~lib/_gambit#.scm")
(##include "../values/type-macros.scm")

;;; ---------------------------------------------------------------------
;;; The Protocol
;;; ---------------------------------------------------------------------

(%define-protocol PrimitiveValue)
