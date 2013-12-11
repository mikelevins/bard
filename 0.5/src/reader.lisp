;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          reader.lisp
;;;; Project:       Bard
;;;; Purpose:       the bard reader
;;;; Author:        mikel evins
;;;; Copyright:     2013 mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard)

;;; ---------------------------------------------------------------------
;;; bard readtable
;;; ---------------------------------------------------------------------

(defparameter *default-readtable* (copy-readtable))

(defparameter *bard-readtable*
  (let ((tbl (copy-readtable *default-readtable*)))
    (setf (readtable-case tbl) :preserve)
    tbl))

;;; ---------------------------------------------------------------------
;;; character reader
;;; ---------------------------------------------------------------------

(set-macro-character
 #\\
 #'(lambda (stream ch)
     (let ((ch-reader (get-dispatch-macro-character #\# #\\ *default-readtable*)))
       (funcall ch-reader stream ch nil)))
 nil
 *bard-readtable*)

;;; ---------------------------------------------------------------------
;;; bard read
;;; ---------------------------------------------------------------------

(defun bard-read (&optional (stream *standard-input*))
  (let ((*readtable* *bard-readtable*)
        (*package* (find-package :bard)))
    (read stream nil nil nil)))

(defun bard-read-from-string (s)
  (with-input-from-string (in s)
    (bard-read in)))
