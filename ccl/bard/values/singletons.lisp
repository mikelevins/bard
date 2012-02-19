;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          singletons.lisp
;;;; Project:       Bard
;;;; Purpose:       unique Bard values
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:bard)

(defun nothing? (x)(eql x nil))
(defun something? (x)(not (nothing? x)))

(defun nothing () nil)

;;; (nothing)

(defclass false ()())
(defmethod print-object ((x false)(out stream))
  (format out "false"))

(let (($false nil))
  (defun false ()
    (or $false
        (progn
          (setf $false (make-instance 'false))
          $false))))

(defun true () t)
(defmethod false? (x) nil)
(defmethod false? ((x null)) t)
(defmethod false? ((x false)) t)
(defmethod true? (x) (not (false? x)))


;;; (false)
;;; (false? (false))
;;; (false? nil)
;;; (false? (true))
;;; (true? (true))
;;; (true? 0)
