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

(defprim 'bard-symbols::< 2
    (make-prim :name 'bard-symbols::<
               :n-args 2
               :opcode 'bard::bard<
               :always t
               :side-effects nil))

(defprim 'bard-symbols::> 2
    (make-prim :name 'bard-symbols::>
               :n-args 2
               :opcode 'bard::bard>
               :always t
               :side-effects nil))

(defprim 'bard-symbols::<= 2
    (make-prim :name 'bard-symbols::<=
               :n-args 2
               :opcode 'bard::bard<=
               :always t
               :side-effects nil))

(defprim 'bard-symbols::>= 2
    (make-prim :name 'bard-symbols::>=
               :n-args 2
               :opcode 'bard::bard>=
               :always t
               :side-effects nil))

(defprim 'bard-symbols::|identical?| 2
    (make-prim :name 'bard-symbols::|identical?|
               :n-args 2
               :opcode 'bard::identical?
               :always t
               :side-effects nil))

(defprim 'bard-symbols::= 2
    (make-prim :name 'bard-symbols::=
               :n-args 2
               :opcode 'bard::equal?
               :always t
               :side-effects nil))
