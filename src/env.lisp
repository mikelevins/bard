;;;; ***********************************************************************
;;;;
;;;; Name:          env.lisp
;;;; Project:       the bard programming lnaguage
;;;; Purpose:       representing environments
;;;; Author:        mikel evins
;;;; Copyright:     2025 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard)

(defun set-var! (var val env)
  "Set a variable to a value, in the given or global environment."
  (if (assoc var env)
      (setf (second (assoc var env)) val)
      (set-global-var! var val))
  val)

(defun get-var (var env)
  "Get the value of a variable, from the given or global environment."
    (if (assoc var env)
        (second (assoc var env))
        (get-global-var var)))

(defun set-global-var! (var val)
  (setf (get var 'global-val) val))

(defvar +unbound+ (make-symbol "+UNBOUND+"))

(defun get-global-var (var)
  (let* ((default +unbound+)
         (val (get var 'global-val default)))
    (if (eq val default)
        (error "Unbound bard variable: ~a" var)
        val)))

(defun extend-env (vars vals env)
  "Add some variables and values to an environment."
  (nconc (mapcar #'list vars vals) env))
