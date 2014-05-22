;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          primitives-types.lisp
;;;; Project:       Bard
;;;; Purpose:       types primitives
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

(defprim 'bard-symbols::|exactly| 1
    (make-prim :name 'bard-symbols::|exactly|
               :n-args 1
               :opcode 'bard::singleton
               :always t
               :side-effects nil))
