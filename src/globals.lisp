;;;; -*- Mode: Lisp; Syntax: Common-Lisp -*-
;;;; ---------------------------------------------------------------------
;;;; bardvm
;;;; A VM implementation based on Norvig's Scheme compiler from PAIP
;;;; globals.lisp
;;;; global variables used by the compiler and vm
;;;; ---------------------------------------------------------------------

(in-package :bardvm)

;;;; ---------------------------------------------------------------------
;;;; globals
;;;; ---------------------------------------------------------------------

(defvar *label-num* 0)
(defvar *bard-readtable* (copy-readtable))
