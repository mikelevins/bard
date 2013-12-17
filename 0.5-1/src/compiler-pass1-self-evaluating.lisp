;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          compiler-pass1-self-evaluating.lisp
;;;; Project:       Bard
;;;; Purpose:       compiling self-evaluating forms
;;;; Author:        mikel evins
;;;; Copyright:     2013 mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard)

(defun self-evaluating? (exp &optional (env nil))
  (or (null exp)
   (and (atom exp)
        (not (named-constant? exp))
        (or (keywordp exp)
            (not (symbolp exp))))))

(defmethod comp1-self-evaluating (exp &optional (env nil))
  exp)

(defmethod comp1-self-evaluating ((exp null) &optional (env nil))
  '(%nothing%))

(defmethod comp1-self-evaluating ((exp %undefined%) &optional (env nil))
  '(%undefined%))

(defmethod comp1-self-evaluating ((exp %true%) &optional (env nil))
  '(%true%))

(defmethod comp1-self-evaluating ((exp %false%) &optional (env nil))
  '(%false%))

(defmethod comp1-self-evaluating ((exp %eof%) &optional (env nil))
  '(%eof%))
