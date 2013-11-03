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

(defun make-bindings (vars vals)
  (assert (= (length vars)(length vals))() 
          "Wrong number of arguments; expected ~a, but found ~a" (length vars) (length vals))
  (mapcar 'cons vars vals))

(defun merge-bindings (b1 b2)
  (append b2 b1))

(defclass <environment> ()
  ((bindings :accessor bindings :initform nil :initarg :bindings)))

(defun null-environment () 
  (make-instance '<environment>))

(defun make-environment (vars vals) 
  (make-instance '<environment> :bindings (make-bindings vars vals)))

(defmethod in-environment? ((var symbol) (env <environment>))
  (let ((entry (assoc var (bindings env))))
    (and entry t)))

(defmethod env-ref ((env <environment>) (var symbol))
  (let ((entry (assoc var (bindings env))))
    (if entry (cdr entry) *undefined*)))

(defmethod env-set! ((env <environment>) (var symbol) val)
  (let* ((entry (assoc var (bindings env))))
    (if entry
        (setf (cdr entry) val)
        (error "Undefined variable ~a" (value->literal-string var)))))

(defmethod extend-environment ((env <environment>)(vars list)(vals list))
  (let* ((old-bindings (bindings env))
         (new-bindings (merge-bindings old-bindings (make-bindings vars vals))))
    (make-instance '<environment> :bindings new-bindings)))

(defmethod merge-environments ((env1 <environment>)(env2 <environment>))
  (make-instance '<environment> :bindings (merge-bindings (bindings env1) (bindings env2))))

(defmethod merge-environments ((env1 null)(env2 <environment>))
  (declare (ignore env1))
  env2)

(defmethod merge-environments ((env1 <environment>)(env2 null))
  (declare (ignore env2))
  env1)

