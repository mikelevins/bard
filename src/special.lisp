;;;; ***********************************************************************
;;;;
;;;; Name:          special.lisp
;;;; Project:       the Bard language
;;;; Purpose:       bard core special forms
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard.core)

(defmacro ^ (lambda-list &body body)
  `(lambda ,lambda-list ,@body))

(defmacro $ (fn &rest args)
  `(funcall ,fn ,@args))

(defmacro |begin| (name &rest args)
  `(block ,name ,@args))

(defmacro |cond| (&rest args)
  `(cond ,@args))

(defmacro |if| (test then &optional (else nil))
  `(if ,test ,then ,else))

(defmacro |set!| (place val)
  `(setf ,place ,val))

