;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          env.lisp
;;;; Project:       Bard
;;;; Purpose:       variable-binding environments
;;;; Author:        mikel evins
;;;; Copyright:     2011 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard)

(defclass environment ()
  ((entries :reader entries :initform (fset:empty-map (undefined)) :initarg :entries)))

(defmethod getvar ((e environment)(var name))
  (fset:lookup (entries e)(variable-name var)))

(defun empty-environment ()
  (make-instance 'environment))

(defun standard-environment ()
  (empty-environment))