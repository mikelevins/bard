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
;;; A variable-binding environment consists of a chain of frames.
;;; Each frame contains a vector of binding cells.
;;; Each cell contains a value.
;;; Each frame also contains a parallel vector of variable-names,
;;; with each name corresponding to a cell. These variable names
;;; are recorded to assist with debugging; compiled code normally
;;; accesses variable values using integer indexes into frames.
;;; Variable references in source code are compiled to pairs of
;;; indexes: the first index to select the frame; the second to
;;; select the binding within the frame.

(in-package :bint)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass unbound-value ()())
  (defparameter $unbound (make-instance 'unbound-value)))

(defclass environment ()
  ((environments :reader environments)
   (bindings :accessor bindings :initarg :bindings)
   (varnames :accessor varnames :initarg :varnames)))

(defmethod environments ((x (eql nil)))
  nil)

(defmethod initialize-instance :after ((e environment) &key (next-environment nil) &allow-other-keys)
  (let* ((envs (cons e (coerce (environments next-environment) 'list)))
         (env-count (length envs))
         (envs-vector (make-array env-count :initial-contents envs)))
    (setf (slot-value e 'environments) envs-vector)))

(defmethod get-value ((env environment)(frame-index integer)(cell-index integer))
  (let ((frame (elt (environments env) frame-index)))
    (elt (bindings frame) cell-index)))

(defmethod set-value! ((env environment)(frame-index integer)(cell-index integer) val)
  (let ((frame (elt (environments env) frame-index)))
    (setf (elt (bindings frame) cell-index) val)))

;;; creates a new environment whose next environment is the first
;;; argument, and which has the additional variable bindings listed in
;;; bindings. bindings is a plist of alternating varnames and values
(defun extend-environment (e &rest bindings)
  (assert (evenp (length bindings))()
          "Odd number of elements in initial bindings")
  (let* ((indexes (util::range 0 (length bindings)))
         (var-indexes (util::filter 'evenp indexes))
         (val-indexes (util::filter 'oddp indexes))
         (varnames (mapcar (lambda (i) (elt bindings i))
                           var-indexes))
         (vals (mapcar (lambda (i) (elt bindings i))
                           val-indexes)))
    (make-instance 'environment
                   :varnames (make-array (length varnames) :initial-contents varnames)
                   :bindings (make-array (length vals) :initial-contents vals)
                   :next-environment e)))

;;; adds a new variable-binding to the given environment
;;; the new binding cannot use a varname that is already used in this environment
;;; it can, however, shadow a variable binding with the same name in an enclosing 
;;; environment
(defmethod add-binding! ((env environment)(varname cl:symbol) &optional (val $unbound))
  (assert (not (some (lambda (vn) (eql varname vn))
                     (varnames env)))
          ()
          "A binding already exists for the variable ~A in the current environment"
          varname)
  (let* ((old-varnames (coerce (varnames env) 'list))
         (old-bindings (coerce (bindings env) 'list))
         (new-count (1+ (length old-bindings)))
         (new-varnames (make-array new-count :initial-contents (append old-varnames (list varname))))
         (new-bindings (make-array new-count :initial-contents (append old-bindings (list val)))))
    (setf (varnames env) new-varnames)
    (setf (bindings env) new-bindings)
    env))
