;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          primitives-structures.lisp
;;;; Project:       Bard
;;;; Purpose:       structures primitives
;;;; Author:        mikel evins
;;;; Copyright:     2013 mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard)

;;; ---------------------------------------------------------------------
;;; primitive definitions
;;; ---------------------------------------------------------------------

(defprim 'bard-symbols::|get-structure| 1
    (make-prim :name 'bard-symbols::|get-structure|
               :n-args 1
               :opcode 'bard::get-structure
               :always t
               :side-effects nil))
