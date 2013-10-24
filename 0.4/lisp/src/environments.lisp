;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          environments.lisp
;;;; Project:       Bard
;;;; Purpose:       representation of lexical environments
;;;; Author:        mikel evins, after Norvig
;;;;
;;;; ***********************************************************************

(in-package :bard)

(defclass environment ()
  ((bindings :accessor bindings :initform nil :initarg :bindings)))

(defmethod extend-environment ((vars cons) (vals cons) (env environment))
  (let* ((bindings (bindings env))
         (bindings* (append (mapcar #'cons vars vals)
                            bindings)))
    (setf (bindings env) bindings*)
    env))

(defmethod in-environment? ((s symbol)(env environment))
  (let* ((bindings (bindings env))
         (frame (find s bindings :test #'find)))
    (if frame
        (values (position frame bindings)
                (position s frame))
        nil)))
