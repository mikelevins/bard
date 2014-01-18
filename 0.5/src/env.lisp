;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          env.lisp
;;;; Project:       Bard
;;;; Purpose:       Bard lexical environments
;;;; Author:        mikel evins
;;;; Copyright:     2014 mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard)

;;; ---------------------------------------------------------------------
;;; 
;;; ---------------------------------------------------------------------

(defun null-bindings ()
  (fset:map :default |undefined|))

(defclass env ()
  ((bindings :accessor %bindings :initform (null-bindings) :initarg :bindings)))

(defun null-env ()
  (make-instance 'env :bindings (null-bindings)))

(defmethod lref ((env env) (key symbol))
  (fset:lookup (%bindings env) key))

(defmethod lset ((env env) (key symbol) val)
  (setf (%bindings env)
        (fset:with (%bindings env) key val)))

(defmethod bound? ((env env) (key symbol))
  (multiple-value-bind (val found?)(fset:lookup (%bindings env) key)
    found?))

(defmethod extend-env ((env env) &rest plist)
  (if (null plist)
      env
      (if (null (cdr plist))
          (error "Odd number of arguments to extend-env: ~S" plist)
          (let ((k (car plist))
                (v (cadr plist)))
            (cl:apply #'extend-env
                      (make-instance 'env :bindings (put-key (%bindings env) k v))
                      (cddr plist))))))
