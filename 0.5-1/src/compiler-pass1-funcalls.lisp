;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          compiler-pass1-funcalls.lisp
;;;; Project:       Bard
;;;; Purpose:       compiling function calls
;;;; Author:        mikel evins
;;;; Copyright:     2013 mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard)

(defun comp1-funcall (exp &optional (env nil))
  (let ((op (car exp))
        (args (cdr exp)))
    `(bard-funcall ,(comp1 op env)
                   ,@(mapcar (lambda (arg)(comp1 arg env))
                             args))))
