;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          special.lisp
;;;; Project:       Bard
;;;; Purpose:       bard special forms
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(cl:in-package #:bard)

;;; =====================================================================
;;; Special Forms
;;; =====================================================================

;;; ->
;;; ----------------------------------------------------------------------

(cl:defmacro -> (args cl:&rest funs)
  (cl:labels ((gen-vars  (n)
                (cl:loop for x from 0 below n
                         collect (cl:gensym))))
    (cl:if (cl:null funs)
           `(cl:values ,@args)
           (cl:let ((f (cl:car funs))
                    (more-funs (cl:cdr funs))
                    (vars (gen-vars (cl:length args))))
             (cl:if (cl:null more-funs)
                    `(cl:multiple-value-bind ,vars (cl:funcall ,f ,@args)
                       (cl:values ,@vars))
                    `(cl:multiple-value-bind ,vars (cl:funcall ,f ,@args)
                       (-> ,vars ,@more-funs)))))))

;;; begin
;;; ----------------------------------------------------------------------

(cl:defmacro begin (cl:&body body)
  `(cl:progn ,@body))

;;; cond
;;; ----------------------------------------------------------------------

(cl:defmacro cond (cl:&body body)
  `(cl:cond ,@body))

;;; define
;;; ----------------------------------------------------------------------

;;; define-function
;;; ----------------------------------------------------------------------

;;; function
;;; ----------------------------------------------------------------------

;;; generate
;;; ----------------------------------------------------------------------

;;; if
;;; ----------------------------------------------------------------------

(cl:defmacro if (cl:&body body)
  `(cl:if ,@body))

;;; let
;;; ----------------------------------------------------------------------

;;; loop
;;; ----------------------------------------------------------------------

;;; match
;;; ----------------------------------------------------------------------

;;; method
;;; ----------------------------------------------------------------------

;;; not
;;; ----------------------------------------------------------------------

(cl:defmacro not (x)
  `(cl:not ,x))

;;; quasiquote
;;; ----------------------------------------------------------------------

;;; quote
;;; ----------------------------------------------------------------------

;;; set!
;;; ----------------------------------------------------------------------

;;; setter
;;; ----------------------------------------------------------------------

;;; time
;;; ----------------------------------------------------------------------

;;; values
;;; ----------------------------------------------------------------------

(cl:defmacro values (cl:&body body)
  `(cl:values ,@body))

;;; with-exit
;;; ----------------------------------------------------------------------


