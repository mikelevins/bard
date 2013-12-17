;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          compiler-pass1-setters.lisp
;;;; Project:       Bard
;;;; Purpose:       compiling setter expressions
;;;; Author:        mikel evins
;;;; Copyright:     2013 mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard)

(defun setter-form? (exp)
  (and (listp exp)
       (cdr exp)
       (eql 'bard-symbols::|setter| (car exp))))

(defun comp1-setter-form (exp &optional (env nil))
  (assert (and (cdr exp)(not (cddr exp)))()
          "Malformed setter expression: ~s" exp)
  `(setter ,(second exp)))
