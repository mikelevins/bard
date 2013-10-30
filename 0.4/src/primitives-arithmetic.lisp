;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          primitives-arithmetic.lisp
;;;; Project:       Bard
;;;; Purpose:       arithmetic primitives for Bard
;;;; Author:        mikel evins
;;;; Copyright:     2013 mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard)

(defprim 'bard-symbols::+ 2
    (make-prim :name 'bard-symbols::+
               :n-args 2
               :opcode 'cl:+
               :always t
               :side-effects nil))

(defprim 'bard-symbols::* 2
    (make-prim :name 'bard-symbols::*
               :n-args 2
               :opcode 'cl:*
               :always t
               :side-effects nil))

(defprim 'bard-symbols::- 2
    (make-prim :name 'bard-symbols::-
               :n-args 2
               :opcode 'cl:-
               :always t
               :side-effects nil))

(defprim 'bard-symbols::/ 2
    (make-prim :name 'bard-symbols::/
               :n-args 2
               :opcode 'cl:/
               :always t
               :side-effects nil))
