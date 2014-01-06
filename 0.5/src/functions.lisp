;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          classes.lisp
;;;; Project:       Bard
;;;; Purpose:       representation of Bard functions
;;;; Author:        mikel evins
;;;; Copyright:     2014 mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard)

;;; ---------------------------------------------------------------------
;;; function representation
;;; ---------------------------------------------------------------------

(defclass bard-function ()
  ((input-types :accessor %input-types :initarg :input-types)
   (output-types :accessor %output-types :initarg :output-types)))

(defmethod print-object ((obj bard-function)(out stream))
  (princ "(->" out)
  (dolist (it (%input-types obj))
    (princ #\space out)
    (princ it out))
  (princ " ->" out)
  (dolist (it (%output-types obj))
    (princ #\space out)
    (princ it out))
  (princ ")" out))

(defmacro -> (&rest elts)
  (let* ((arrow-pos (position '-> elts)))
    (if arrow-pos 
        (let ((inputs (subseq elts 0 arrow-pos))
              (outputs (subseq elts (1+ arrow-pos))))
          `(make-instance 'bard-function :input-types ',inputs :output-types ',outputs))
        (error "Invalid function syntax: ~S" elts))))

