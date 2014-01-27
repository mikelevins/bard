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

(defun null-env () (cons :env nil))

(defmethod find-variable ((var symbol)(env cons)) 
  (let ((binding (assoc var (cdr env))))
    (if binding
        (cdr binding)
        nil)))

(defmethod extend-env ((env cons) var val &rest more) 
  (if more
      (apply #'extend-env
             (cons :env
                   (cons (cons var val)
                         (cdr env)))
             more)
      (cons :env
            (cons (cons var val)
                  (cdr env)))))

