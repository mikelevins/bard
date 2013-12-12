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
;;; bard read
;;; ---------------------------------------------------------------------

(defun bard-read (&optional (stream *standard-input*)(eof nil))
  (let ((*readtable* *bard-readtable*)
        (*package* (find-package :bard)))
    (read stream nil eof nil)))

(defun bard-read-from-string (s)
  (with-input-from-string (in s)
    (bard-read in)))

(defmethod bard-read-convert ((in stream)(out stream))
  (let* ((eof (gensym)))
    (loop for obj = (bard-read in eof)
       until (eql obj eof)
       do (format out "~S~%" obj))
    (finish-output out)))

(defmethod bard-read-convert ((path pathname) out)
  (with-open-file (in path :direction :input)
    (bard-read-convert in out)))

(defmethod bard-read-convert (in (path pathname))
  (with-open-file (out path :direction :output)
    (bard-read-convert in out)))

(defmethod bard-read-convert ((path string) out)
  (bard-read-convert (pathname path) out))

;;; test function:
;;; writes the s-expressions obtained by running the bard reader on a source file
(defmethod bard-read-convert (in (path string))
  (bard-read-convert in (pathname path)))

;;; (bard-read-convert "/Users/mikel/Workshop/bard/0.5/testdata/namer.bard" "/Users/mikel/Workshop/bard/0.5/testdata/namer.bardo")
