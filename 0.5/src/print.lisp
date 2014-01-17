;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          print.lisp
;;;; Project:       Bard
;;;; Purpose:       Bard printer
;;;; Author:        mikel evins
;;;; Copyright:     2014 mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard)

;;; ---------------------------------------------------------------------
;;; 
;;; ---------------------------------------------------------------------

(defmethod bard-print (obj &optional (out *standard-output*))
  (prin1 obj out)
  obj)

(defmethod bard-print ((obj null) &optional (out *standard-output*))
  (princ "nothing" out)
  obj)

(defmethod bard-print ((obj (eql '|undefined|)) &optional (out *standard-output*))
  (princ "undefined" out)
  obj)

(defmethod bard-print ((obj (eql '|true|)) &optional (out *standard-output*))
  (princ "true" out)
  obj)

(defmethod bard-print ((obj (eql '|false|)) &optional (out *standard-output*))
  (princ "false" out)
  obj)

(defmethod bard-print ((obj symbol) &optional (out *standard-output*))
  (if (keywordp obj)
      (progn
        (princ #\: out)
        (princ (symbol-name obj) out))
      (princ (symbol-name obj) out))
  obj)

(defmethod bard-print ((obj fset:seq) &optional (out *standard-output*))
  (princ #\( out)
  (unless (fset:empty? obj)
    (let ((head (fset:first obj))
          (tail (fset:less-first obj)))
      (bard-print head out)
      (unless (fset:empty? tail)
        (loop for i from 0 below (fset:size tail)
           do (progn
                (princ #\space out)
                (bard-print (fset:@ tail i) out))))))
  (princ #\) out)
  obj)


(defmethod bard-print ((obj fset:map) &optional (out *standard-output*))
  (princ #\{ out)
  (unless (fset:empty? obj)
    (let ((keys (fset:convert 'list (keys obj))))
      (dolist (key keys)
        (princ #\space out)
        (bard-print key out)
        (princ #\space out)
        (bard-print (get-key obj key) out))))
  (princ #\space out)
  (princ #\} out)
  obj)
