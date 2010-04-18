;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          environment.lisp
;;;; Project:       Bard - a modern Lisp
;;;; Purpose:       variable-binding environments
;;;; Author:        mikel evins
;;;; Copyright:     2010 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard)

;;; ============================================================
;;; BARD environments
;;; ============================================================

(defclass unbound-variable ()()(:metaclass singleton-class))
(defun unbound-variable ()(make-instance 'unbound-variable))

(defun null-environment () nil)

(defun add-binding (env var val)
  (acons var val env))

(defun find-binding (env var)
  (assoc var env :test '=))

(defun lookup-variable (var env)
  (let ((binding (find-binding env var)))
    (or (and binding (cdr binding))
        (unbound-variable))))

(let ((toplevel-env nil))

  (defun alter-toplevel-environment! (new-env)
    (setf toplevel-env new-env)
    (values))

  (defun bard-toplevel-environment () toplevel-env))


