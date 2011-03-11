;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          module.lisp
;;;; Project:       Bard
;;;; Purpose:       definition of modules
;;;; Author:        mikel evins
;;;; Copyright:     2011 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard)

(defclass module ()
  ((module-name :reader module-name :initarg :name)
   (exports :accessor exports :initform nil)
   (variables :accessor variables :initform (fset:empty-map nil))))

(defmethod print-object ((m module)(s stream))
  (princ "#<module " s)
  (princ (module-name m) s)
  (princ ">" s))

(defun make-module (name)
  (let ((name (validate-module-name name)))
    (if name
        (make-instance 'module :name name)
        (error "Invalid module name: ~S" name))))

(defmethod define-variable! ((m module)(name string) &key (value (undefined))(export nil))
  (let ((varbox (box value)))
    (setf (variables m)
          (fset:with (variables m) name varbox))
    (when export
      (pushnew name (exports m) :test #'string=))))

(defmethod get-module-variable ((m module)(nm string))
  (get-box (fset:@ (variables m) nm)))

(defmethod set-module-variable! ((m module)(nm string) val)
  (set-box! (fset:@ (variables m) nm) val))

(defmethod getvar ((m module)(var name))
  (get-module-variable m (variable-name var)))

(defmethod setvar! ((m module)(var name) val)
  (set-module-variable! m (variable-name var) val))

