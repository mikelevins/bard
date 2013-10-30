;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          primitives-names.lisp
;;;; Project:       Bard
;;;; Purpose:       primitives for working with symbols and keywords
;;;; Author:        mikel evins
;;;; Copyright:     2013 mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard)

;;; ---------------------------------------------------------------------
;;; name converters
;;; ---------------------------------------------------------------------

(defmethod as-keyword ((s string))
  (intern s :keyword))

(defmethod as-keyword ((s symbol))
  (intern (symbol-name s) :keyword))

(defmethod as-symbol ((s string))
  (intern s :bard-symbols))

(defmethod as-symbol ((k keyword))
  (intern (symbol-name k) :bard-symbols))

;;; ---------------------------------------------------------------------
;;; primitive definitions
;;; ---------------------------------------------------------------------

(defprim 'bard-symbols::|as-keyword| 1
    (make-prim :name 'bard-symbols::|as-keyword|
               :n-args 1
               :opcode 'bard::as-keyword
               :always t
               :side-effects nil))

(defprim 'bard-symbols::|as-symbol| 1
    (make-prim :name 'bard-symbols::|as-symbol|
               :n-args 1
               :opcode 'bard::as-symbol
               :always t
               :side-effects nil))
