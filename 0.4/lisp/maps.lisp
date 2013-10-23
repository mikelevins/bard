;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          maps.lisp
;;;; Project:       Bard
;;;; Purpose:       representation of maps
;;;; Author:        mikel evins
;;;; Copyright:     2013 mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard)

;;; ---------------------------------------------------------------------
;;; <alist-map>
;;; ---------------------------------------------------------------------

(defclass <alist-map> ()
  ((entries :accessor entries :initform nil :initarg :entries)))

(defmethod alist-map? (x) nil)
(defmethod alist-map? ((x <alist-map>)) t)


;;; ---------------------------------------------------------------------
;;; maps
;;; ---------------------------------------------------------------------

(defun make-map (&rest keys-and-values)
  (let ((entries (loop for (a b) on keys-and-values by #'cddr collecting (cons a b))))
    (make-instance '<alist-map> :entries entries)))

