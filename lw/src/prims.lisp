;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          prims.lisp
;;;; Project:       Bard
;;;; Purpose:       bard primitives
;;;; Author:        mikel evins
;;;; Copyright:     2011 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard)

(defun %primitive-add (arg-seq)
  (let* ((args (mapcar #'data (fset:convert 'cl:list (elements arg-seq))))
         (result (cl:apply #'+ args)))
    (cond
      ((integerp result)(make-instance 'integer :data result))
      ((floatp result)(make-instance 'float :data result))
      (t (error "Unrecognized output type in %primitive-add: ~S (type: ~S)"
                result (type-of result))))))

(defun %primitive-subtract (arg-seq)
  (let* ((args (mapcar #'data (fset:convert 'cl:list (elements arg-seq))))
         (result (cl:apply #'- args)))
    (cond
      ((integerp result)(make-instance 'integer :data result))
      ((floatp result)(make-instance 'float :data result))
      (t (error "Unrecognized output type in %primitive-subtract: ~S (type: ~S)"
                result (type-of result))))))

(defun %primitive-multiply (arg-seq)
  (let* ((args (mapcar #'data (fset:convert 'cl:list (elements arg-seq))))
         (result (cl:apply #'* args)))
    (cond
      ((integerp result)(make-instance 'integer :data result))
      ((floatp result)(make-instance 'float :data result))
      (t (error "Unrecognized output type in %primitive-multiply: ~S (type: ~S)"
                result (type-of result))))))

(defun %primitive-divide (arg-seq)
  (let* ((args (mapcar #'data (fset:convert 'cl:list (elements arg-seq))))
         (result (cl:apply #'/ args)))
    (cond
      ((integerp result)(make-instance 'integer :data result))
      ((floatp result)(make-instance 'float :data result))
      (t (error "Unrecognized output type in %primitive-divide: ~S (type: ~S)"
                result (type-of result))))))

