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

;;; ---------------------------------------------------------------------
;;; arithmetic ops
;;; ---------------------------------------------------------------------

(defun bard< (x y)
  (if (< x y)
      *true*
      *false*))

(defun bard> (x y)
  (if (> x y)
      *true*
      *false*))

(defun bard<= (x y)
  (if (<= x y)
      *true*
      *false*))

(defun bard>= (x y)
  (if (>= x y)
      *true*
      *false*))

;;; ---------------------------------------------------------------------
;;; equivalence predicates
;;; ---------------------------------------------------------------------

(defun identical? (x y)
  (if (eq x y)
      *true*
      *false*))

(defun equal? (x y)
  (if (equal x y)
      *true*
      *false*))

;;; ---------------------------------------------------------------------
;;; primitive definitions
;;; ---------------------------------------------------------------------

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
