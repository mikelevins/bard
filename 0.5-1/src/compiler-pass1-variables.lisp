;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          compiler-pass1-named-constants.lisp
;;;; Project:       Bard
;;;; Purpose:       compiling variable references
;;;; Author:        mikel evins
;;;; Copyright:     2013 mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard)


(defun find-in-env? (exp env)
  (assoc exp env))

(defun comp1-variable-reference (exp &optional (env nil))
  (if (find-in-env? exp env)
      exp
      `(gref ,exp)))
