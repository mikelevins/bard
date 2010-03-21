;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: asdf -*-
;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          apply.lisp
;;;; Project:       Bard - a modern Lisp
;;;; Purpose:       function-application
;;;; Author:        mikel evins
;;;; Copyright:     2010 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard)

;;; ============================================================
;;; 
;;; ============================================================

(defmethod apply (thing other-things)
  (error "Don't know how to apply ~a to ~a" thing other-things))

(defmethod apply ((f cl:function) args)
  (cl:apply f args))

(defmethod apply ((f fset:map) args)
  (let ((default (if (> (count args) 1)
                     (element args 1)
                     (nothing))))
    (get-key f (element args 0) default)))

