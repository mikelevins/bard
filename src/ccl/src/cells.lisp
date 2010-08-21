;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          cells.lisp
;;;; Project:       Bard - a modern Lisp, draft 4
;;;; Purpose:       a cell is a box that holds a value
;;;; Author:        mikel evins
;;;; Copyright:     2010 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package "BARD")

;;; ========================================================================
;;; CLASS: cell
;;; ========================================================================

(defclass cell ()())
(defclass read-only-cell ()((value :reader value :initarg :value :initform (undefined))))
(defclass mutable-cell ()((value :accessor value :initarg :value :initform (undefined))))

(defmethod mutable? ((c read-only-cell)) (declare (ignore c)) nil)
(defmethod read-only? ((c read-only-cell)) (declare (ignore c)) t)

(defmethod mutable? ((c mutable-cell)) (declare (ignore c)) t)
(defmethod read-only? ((c mutable-cell)) (declare (ignore c)) nil)

(defun make-cell (&key (mutable nil))
  (if mutable
      (make-instance 'mutable-cell)
      (make-instance 'read-only-cell)))

