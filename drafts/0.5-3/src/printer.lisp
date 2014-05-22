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

;;; ---------------------------------------------------------------------
;;; low-level object->string
;;; ---------------------------------------------------------------------

(defmethod object->display-string (obj)
  (with-output-to-string (out)
    (princ obj out)))

(defmethod object->display-string ((obj null))
  (with-output-to-string (out)
    (princ "nothing" out)))

(defmethod object->display-string ((obj cl:symbol))
  (with-output-to-string (out)
    (format out "~S" obj)))

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

(defmethod object->display-string ((obj record-instance))
  (with-output-to-string (out)
    (princ "#<record {" out)
    (let* ((slots (slots obj))
           (all-keys (fset:convert 'cl:list (fset:domain slots)))
           (first-key (first all-keys))
           (rest-keys (rest all-keys)))
      (when first-key
        (format out "~S" first-key)
        (princ " " out)
        (format out "~S" (fset:@ slots first-key)))
      (when rest-keys
        (loop for key in rest-keys 
           do (let ((val (fset:@ slots key)))
                (princ " " out)
                (format out "~S" key)
                (princ " " out)
                (format out "~S" val)))))
    (princ "}>" out)))

(defmethod object->display-string ((obj values-instance))
  (with-output-to-string (out)
    (loop for item in (vals obj) 
       do (format out "~%~S" item))))

;;; ---------------------------------------------------------------------
;;; printer entry points
;;; ---------------------------------------------------------------------

(defun display (x) (princ (object->display-string x)))
