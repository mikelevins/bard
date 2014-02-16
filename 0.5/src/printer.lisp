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

(defmethod object->display-string ((obj false))
  (with-output-to-string (out)
    (princ "false" out)))

(defun display (x) (princ (object->display-string x)))
