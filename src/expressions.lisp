;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          expressions.lisp
;;;; Project:       bard
;;;; Purpose:       the bard abstract syntax tree
;;;; Author:        mikel evins
;;;; Requirements:  Clozure Common Lisp
;;;;
;;;; ***********************************************************************

(in-package :bard)

(defclass expression ()()) ; abstract

;;; comments
(defclass comment (expression)())
(defclass line-comment (comment)())
(defclass block-comment (comment)())

;;; atoms
(defclass atom (expression)()) ; abstract

(defclass void-expression (atom)())

(defclass true-expression (atom)())

(defclass false-expression (atom)())

(defclass symbol-expression (atom)
  ((name :reader name :initarg :name)))

(defclass keyword-expression (atom)
  ((name :reader name :initarg :name)))

(defclass character-expression (atom)
  ((value :reader value :initarg :value)))

(defclass numeric-expression (atom)
  ((value :reader value :initarg :value)))

;;; literal sequences
(defclass literal-sequence (expression)
  ((elements :reader elements :initarg :elements)))

;;; ordered sequences
(defclass ordered-sequence (literal-sequence)
  ())

(defclass pair-expression (ordered-sequence)())

(defclass vector-expression (ordered-sequence)
  ())

(defclass bitvector-expression (ordered-sequence)
  ())

(defclass wordvector-expression (ordered-sequence)
  ())

;;; unordered sequences
(defclass unordered-sequence (literal-sequence)()) ; abstract

(defclass bitset-expression (unordered-sequence)
  ())

(defclass object-set-expression (unordered-sequence)
  ())

;;; maps
(defclass map-expression (literal-sequence)()) ; abstract

(defclass hashtable-expression (map-expression)
  ())

(defclass hash-trie-expression (map-expression)
  ())

(defclass vector-map-expression (map-expression)
  ())

(defclass pair-map-expression (map-expression)
  ())

;;; text
(defclass text-expression (literal-sequence)
  ((text :reader text :initarg :text)))

(defclass ascii-text-expression (text-expression)
  ())

(defclass unicode-text-expression (text-expression)())




