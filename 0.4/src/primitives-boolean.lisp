;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          primitives-boolean.lisp
;;;; Project:       Bard
;;;; Purpose:       primitives for working with boolean values
;;;; Author:        mikel evins
;;;; Copyright:     2013 mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard)


(defprim 'bard-symbols::|not| 1
    (make-prim :name 'bard-symbols::|not|
               :n-args 1
               :opcode 'bard::bard-not
               :always t
               :side-effects nil))
