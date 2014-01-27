;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          repl.lisp
;;;; Project:       Bard
;;;; Purpose:       the Bard repl
;;;; Author:        mikel evins
;;;; Copyright:     2014 mikel evins
;;;;
;;;; ***********************************************************************

(in-package :|bard|)

(defparameter *default-listener-prompt-format*
  ccl:*listener-prompt-format*)

(defun bard-listener-prompt-format (stream breaklevel)
  (if (zerop breaklevel)
      (format stream "bard> ")
      (format stream "bard(~d)> " breaklevel))
  nil)

(defun bard ()
  (setf ccl:*listener-prompt-format* #'bard-listener-prompt-format)
  (setf cl:*package* (find-package :|bard|))
  (setf cl:*readtable* *bard-read-table*))

(defun |end-bard| ()
  (setf ccl:*listener-prompt-format* *default-listener-prompt-format*)
  (setf cl:*package* (find-package :cl-user))
  (setf cl:*readtable* *standard-read-table*))

;;; (bard)
;;; (|end-bard|)
