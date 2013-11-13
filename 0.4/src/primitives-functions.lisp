;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          primitives-functions.lisp
;;;; Project:       Bard
;;;; Purpose:       functions primitives
;;;; Author:        mikel evins
;;;; Copyright:     2013 mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard)

;;; ---------------------------------------------------------------------
;;; primitive definitions
;;; ---------------------------------------------------------------------

(defprim 'bard-symbols::|assert-method!| 3
    (make-prim :name 'bard-symbols::|assert-method!|
               :n-args 3
               :opcode 'bard::assert-method!
               :always t
               :side-effects t))

