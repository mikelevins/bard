;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          env.lisp
;;;; Project:       Bard
;;;; Purpose:       lexical environments
;;;; Author:        mikel evins
;;;; Copyright:     2014 mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard)

;;; ---------------------------------------------------------------------
;;; environments
;;; ---------------------------------------------------------------------

(defun null-env () nil)

(defmethod find-variable ((var symbol)(env null)) 
  nil)

(defmethod find-variable ((var symbol)(env cons)) 
  (let ((binding (assoc var env)))
    (if binding
        (cdr binding)
        nil)))

(defmethod extend-env ((env null) var val &rest more) 
  (if more
      (apply #'extend-env
             (cons (cons var val)
                   (cdr env))
             more)
      (cons (cons var val)
            env)))

(defmethod extend-env ((env cons) var val &rest more) 
  (if more
      (apply #'extend-env
             (cons (cons var val)
                   (cdr env))
             more)
      (cons (cons var val)
            env)))

