;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: Rev-Fun-Bind -*-

;;; This file is in the public domain.  It is provided with ABSOLUTELY 
;;; NO WARRANTY.

(in-package :rev-fun-bind)

;;; Macros for function binding, like `labels', `flet', and `macrolet' but with
;;; reversed syntax, e.g.:
;;;
;;; (rlabels (foo z)
;;;   (foo (x) ... )
;;;   (bar (y) ... ))
;;;
;;; This can be easier to read (you don't have to write the program in Pascal
;;; order) and indents better too.

(defmacro rlabels (value-form &body clauses)
  "Just like `labels' except that the first subform is the form that computes the
value, and the remaining subforms are the binding clauses."
  ;; The expansion is simpler than the documentation.
  `(labels ,clauses ,value-form))

(defmacro rflet (value-form &body clauses)
  "Just like `flet' except that the first subform is the form that computes the
value, and the remaining subforms are the binding clauses."
  `(flet ,clauses ,value-form))

(defmacro rmacrolet (value-form &body clauses)
  "Just like `macrolet' except that the first subform is the form that computes the
value, and the remaining subforms are the binding clauses."
  `(macrolet ,clauses ,value-form))

