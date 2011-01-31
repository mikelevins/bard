;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: asdf -*-
;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          functions.lisp
;;;; Project:       Bard - a modern Lisp
;;;; Purpose:       representation of functions and methods
;;;; Author:        mikel evins
;;;; Copyright:     2010 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard)

(defclass function ()())

(defclass method (function)())

(defun make-lambda (lambda-list body env)
  (let ((lambda-list (mapcar (lambda (s) (intern (symbol-name (name s)) (get-current-module env)))
                             (fset:convert 'cl:list lambda-list))))
    (let* ((args '(2 3))
           (argcount (length args)))
      (if (= argcount (length lambda-list))
          (let ((lambda-env env))
            (dotimes (i argcount)
              (setf lambda-env
                    (add-binding lambda-env
                                 (elt lambda-list i)
                                 (compile (elt args i) lambda-env))))
            (let* ((exps (fset:convert 'cl:list body))
                   (first-exp (cl:first exps)))
              (error "BUG: find-symbol is not working correctly here")))
          (error "Wrong number of args to function")))))

(defun make-method (exp env)
  (make-lambda (element exp 1)
               (drop 2 exp)
               env))