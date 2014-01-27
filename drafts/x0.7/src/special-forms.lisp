;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          special-forms.lisp
;;;; Project:       Bard
;;;; Purpose:       bard's special forms
;;;; Author:        mikel evins
;;;; Copyright:     2013 mikel evins
;;;;
;;;; ***********************************************************************

(in-package :|bard|)

(defmacro |begin| (&body body)
  `(cl:progn ,@body))

(defmacro |if| (test then else)
  `(cl:if (true? ,test) ,then ,else))

(defmacro |unless| (test &body body)
  `(cl:unless (true? ,test) ,@body))

(defmacro |when| (test &body body)
  `(cl:when (true? ,test) ,@body))


