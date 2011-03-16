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

(defmethod import-variable! ((src-module module)(name string)(dest-module module) 
                             &key (as nil)(export nil)(replace nil))
  (if (member name (exports src-module) :test #'string=)
      (let ((varbox (fset:@ (variables src-module) name))
            (varname (if as as name))
            (already (fset:@ (variables dest-module) varname)))
        (if (and already (not replace))
            (error "Variable ~A already exists in module ~A" name (module-name dest-module))
            (progn
              (setf (variables dest-module)
                    (fset:with (variables dest-module) name varbox))
              (if export
                  (pushnew name (exports dest-module) :test #'string=)
                  (setf (exports dest-module)
                        (remove name (exports dest-module) :test #'string=)))))
        name)
      (error "Variable ~A is not exported by module ~A" name (module-name src-module))))

(defmethod get-module-variable ((m module)(nm string))
  (get-box (fset:@ (variables m) nm)))

(defmethod set-module-variable! ((m module)(nm string) val)
  (set-box! (fset:@ (variables m) nm) val))

(defmethod getvar ((m module)(var name))
  (get-module-variable m (variable-name var)))

(defmethod setvar! ((m module)(var name) val)
  (set-module-variable! m (variable-name var) val))

