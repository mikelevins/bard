;;;; -*- Mode: Lisp; Syntax: Common-Lisp -*-
;;;; ---------------------------------------------------------------------
;;;; bardvm
;;;; A VM implementation based on Norvig's Scheme compiler from PAIP
;;;; globals.lisp
;;;; global variables used by the compiler and vm
;;;; ---------------------------------------------------------------------
;;;; Code from Paradigms of Artificial Intelligence Programming
;;;; Copyright (c) 1991 Peter Norvig
;;;; integrated into bardvm by mikel evins, mikel@evins.net, July 2020

(in-package :bardvm)

;;;; ---------------------------------------------------------------------
;;;; globals
;;;; ---------------------------------------------------------------------

(defvar *label-num* 0)
(defvar *bard-readtable* (copy-readtable))
