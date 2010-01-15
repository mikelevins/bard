;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: asdf -*-
;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          environments.lisp
;;;; Project:       Bard - a near-minimal Cocoa application
;;;; Purpose:       variable-binding environments
;;;; Author:        mikel evins
;;;; Copyright:     2009 by mikel evins
;;;;
;;;; ***********************************************************************

;;; ------------------------------------------------------------------------
;;; ABOUT
;;; ------------------------------------------------------------------------
;;; ;;; A variable-binding environment maps variable names to values.  for
;;; simplicity, we use alists in this implementation.  extending an
;;; environment means creating a new environment that includes the bindings
;;; in an existing environment.  when we do that, we want it to be true
;;; that any bindings added to the original environment after the creation
;;; of the new one are visible in the new one, as long as the new one
;;; doesn't shadow them. In order to manage that, an environment alist is
;;; wrapped in a class that keeps are reference to the original environment.
;;; setting or adding a binding in the new environment affects the alist
;;; of bindings in that environment. setting or adding a binding in the
;;; original environment affects the original environment, but the change
;;; will be visible in the new one as well, as long as the new environment
;;; doesn't shadow the affected binding. this means, though, that variable
;;; lookup must traverse the included environment, if a binding is not found
;;; in the initial environment.


(in-package :bint)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass unbound-value ()())
  (defparameter $unbound (make-instance 'unbound-value))
  (defun unbound () $unbound)
  (defmethod unbound? (x)(declare (ignore x)) nil)
  (defmethod unbound? ((x unbound-value)) t))

(defclass environment ()
  ((bindings :accessor bindings :initarg :bindings)
   (parent :reader parent :initarg :parent)))

(defmethod get-binding ((e list)(s cl:symbol))
  (let ((binding (assq s (bindings e))))
    (if binding
      binding
      (if (parent e)
        (get-binding (parent e) s)
        nil))))

(defmethod get-value ((env environment)(varname cl:symbol))
  (let ((binding (get-binding env varname)))
    (if binding
      (cdr binding)
      (unbound))))

(defmethod set-value! ((env environment)(varname cl:symbol) v)
  (let ((binding (get-binding env varname)))
    (if binding
      (setf (cdr binding) v)
      (setf (bindings env)
            (cons (cons varname v)
                  (bindings env))))))

(defmethod extend-environment ((env null)(bindings list))
  (make-instance 'environment
    :parent nil
    :bindings (mapcar (lambda (e) 
                        (assert (symbolp (car e))()
                                "Invalid variable name: ~S" (car e))
                        (cons (car e)(cdr e)))
                      bindings)))

(defmethod extend-environment ((env environment)(bindings list))
  (make-instance 'environment
    :parent env
    :bindings (mapcar (lambda (e) 
                        (assert (symbolp (car e))()
                                "Invalid variable name: ~S" (car e))
                        (cons (car e)(cdr e)))
                      bindings)))

;;; define-variable differs from set-value! in what happens if
;;; no variable with the supplied name is found in the supplied
;;; environment. set-value! tries to find such a variable in the
;;; parent environment and, if one is found, sets the value in that
;;; environment. define-variable always creates its binding in the
;;; supplied environment, even if an identically-named variable
;;; exists in the parent environment
(defun define-variable (varname val env)
  (let ((binding (assq varname (bindings env))))
    (if binding
      (setf (cdr binding) val)
      (setf (bindings env)
            (cons (cons varname val)
                  (bindings env))))))

