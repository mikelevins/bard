;;;; ***********************************************************************
;;;;
;;;; Name:          compiler.scm
;;;; Project:       Bard
;;;; Purpose:       the bard compiler
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(declare (extended-bindings))

;;; ---------------------------------------------------------------------
;;; ABOUT
;;; ---------------------------------------------------------------------
;;; converts expressions in bard to kernel expressions
;;; by folding constant expressions, expanding all macros,
;;; and rewriting expressions that use the more complex
;;; and powerful constructs of full bard as expressions
;;; that use only the kernel language.

