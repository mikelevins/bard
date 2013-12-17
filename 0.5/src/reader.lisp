;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          reader.lisp
;;;; Project:       Bard
;;;; Purpose:       the Bard reader
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

;;; list literal reader

(set-syntax-from-char #\[ #\( *bard-readtable* *default-readtable*)
(set-syntax-from-char #\] #\) *bard-readtable* *default-readtable*)

(set-macro-character 
 #\[
 (lambda (stream ch)(cons '|bard.language|::|list| (read-delimited-list #\] stream)))
 nil *bard-readtable*)

(set-syntax-from-char #\{ #\( *bard-readtable* *default-readtable*)
(set-syntax-from-char #\} #\) *bard-readtable* *default-readtable*)

(set-macro-character 
 #\{
 (lambda (stream ch)(cons '|bard.language|::|map| (read-delimited-list #\} stream)))
 nil *bard-readtable*)

(set-dispatch-macro-character 
 #\# #\<
 (lambda (stream ch numarg)
   (let* ((token (ccl::read-symbol-token stream))
          (constraint-name (concatenate 'string "<" token))
          (constraint (intern constraint-name :bard-symbols))
          (expr (bard-read stream)))
     `(bard-symbols::|as| ,constraint ,expr)))
 *bard-readtable*)

;;; ---------------------------------------------------------------------
;;; bard read
;;; ---------------------------------------------------------------------

(defun bard-read (&optional (stream *standard-input*)(eof nil))
  (let ((*readtable* *bard-readtable*)
        (*package* (find-package :bard-symbols)))
    (read stream nil eof nil)))

(defun bard-read-from-string (s)
  (with-input-from-string (in s)
    (bard-read in)))

;;; ---------------------------------------------------------------------
;;; reader tests
;;; ---------------------------------------------------------------------

(defun test-read (path)
  (let ((out-path (merge-pathnames (make-pathname :type "log") path)))
    (with-log (out out-path)
      (with-open-file (in path :direction :input)
        (loop for obj = (bard-read in :eof)
           until (eq obj :eof)
           do (let ((*package* (find-package :keyword)))
                (write obj :stream out :escape t :readably t)
                (terpri out)))))))

;;; (test-read "/Users/mikel/Workshop/bard/0.5/testdata/literals.bard")
;;; (test-read "/Users/mikel/Workshop/bard/0.5/testdata/namer.bard")
;;; (test-read "/Users/mikel/Workshop/bard/0.5/testdata/programs.bard")
