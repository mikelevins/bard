;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          primitives-io.lisp
;;;; Project:       Bard
;;;; Purpose:       primitives for input and output
;;;; Author:        mikel evins
;;;; Copyright:     2013 mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard)

;;; ---------------------------------------------------------------------
;;; I/O ops
;;; ---------------------------------------------------------------------


(defun display (x) (princ x))
(defun bard-write (x) (princ (value->literal-string x)))
(defun newline () (terpri))

;;; ---------------------------------------------------------------------
;;; primitive definitions
;;; ---------------------------------------------------------------------

(defprim 'bard-symbols::|display| 1
    (make-prim :name 'bard-symbols::|display|
               :n-args 1
               :opcode 'bard::display
               :always nil
               :side-effects t))

(defprim 'bard-symbols::|newline| 0
    (make-prim :name 'bard-symbols::|newline|
               :n-args 0
               :opcode 'bard::newline
               :always nil
               :side-effects t))

(defprim 'bard-symbols::|read| 0
    (make-prim :name 'bard-symbols::|read|
               :n-args 0
               :opcode 'bard::bard-read
               :always nil
               :side-effects t))

(defprim 'bard-symbols::|write| 1
    (make-prim :name 'bard-symbols::|write|
               :n-args 1
               :opcode 'bard::bard-write
               :always nil
               :side-effects t))
