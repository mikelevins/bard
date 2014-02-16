;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          printer.lisp
;;;; Project:       Bard
;;;; Purpose:       the bard printer
;;;; Author:        mikel evins
;;;; Copyright:     2014 mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard)

(defmethod object->display-string (obj)
  (with-output-to-string (out)
    (princ obj out)))

(defmethod object->display-string ((obj null))
  (with-output-to-string (out)
    (princ "nothing" out)))

(defmethod object->display-string ((obj undefined))
  (with-output-to-string (out)
    (princ "undefined" out)))

(defmethod object->display-string ((obj true))
  (with-output-to-string (out)
    (princ "true" out)))

(defmethod object->display-string ((obj (eql cl:t)))
  (with-output-to-string (out)
    (princ "true" out)))

(defmethod object->display-string ((obj false))
  (with-output-to-string (out)
    (princ "false" out)))

(defmethod object->display-string ((obj end))
  (with-output-to-string (out)
    (princ "end" out)))

(defmethod object->display-string ((obj base-type))
  (with-output-to-string (out)
    (princ "#<base-type " out)
    (princ (type-name obj) out)
    (princ ">" out)))

(defun display (x) (princ (object->display-string x)))
