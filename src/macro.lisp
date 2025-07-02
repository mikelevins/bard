;;;; ***********************************************************************
;;;;
;;;; Name:          macro.lisp
;;;; Project:       the bard programming lnaguage
;;;; Purpose:       macro definition
;;;; Author:        mikel evins
;;;; Copyright:     2025 by mikel evins
;;;; Code from Paradigms of AI Programming
;;;; Copyright (c) 1991 Peter Norvig
;;;  File: interp1.lisp: simple Bard interpreter, including macros.
;;;;
;;;; ***********************************************************************

(in-package :bard)


(defun bard-macro (symbol)
  (and (symbolp symbol) (get symbol 'bard-macro)))

(defmacro def-bard-macro (name parmlist &body body)
  "Define a Bard macro."
  `(setf (get ',name 'bard-macro)
         #'(lambda ,parmlist .,body)))

(defun bard-macro-expand (x)
  "Macro-expand this Bard expression."
  (if (and (listp x) (bard-macro (first x)))
      (bard-macro-expand
        (apply (bard-macro (first x)) (rest x)))
      x))
