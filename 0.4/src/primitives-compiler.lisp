;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          primitives-compiler.lisp
;;;; Project:       Bard
;;;; Purpose:       compiler primitives
;;;; Author:        mikel evins
;;;; Copyright:     2013 mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard)

;;; ---------------------------------------------------------------------
;;; primitive definitions
;;; ---------------------------------------------------------------------

(defprim 'bard-symbols::|compiler| 1
    (make-prim :name 'bard-symbols::|compiler|
               :n-args 1
               :opcode 'bard::compiler
               :always t
               :side-effects nil))
