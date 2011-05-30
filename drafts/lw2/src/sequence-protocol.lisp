;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          sequence-protocol.lisp
;;;; Project:       Bard
;;;; Purpose:       built-in sequence protocol
;;;; Author:        mikel evins
;;;; Copyright:     2011 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard)

(defmethod first ((s sequence))
  (fset:@ (elements s) 0))

(defmethod rest ((s sequence))
  (make-instance 'sequence :elements (fset:less-first (elements s))))

(defmethod length ((s sequence))
  (fset:size (elements s)))

(defmethod map-over ((fn cl:function) (s sequence))
  (let ((result nil))
    (fset:do-seq (e (elements s))
      (setf result (cons (funcall fn e) result)))
    (make-instance 'sequence :elements (fset:convert 'fset:seq (reverse result)))))

