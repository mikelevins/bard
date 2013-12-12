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

;;; list literal reader

(set-syntax-from-char #\[ #\( *bard-readtable* *default-readtable*)
(set-syntax-from-char #\] #\) *bard-readtable* *default-readtable*)

(set-macro-character 
 #\[
 (lambda (stream ch)(cons 'bard::|list| (read-delimited-list #\] stream)))
 nil *bard-readtable*)

(set-syntax-from-char #\{ #\( *bard-readtable* *default-readtable*)
(set-syntax-from-char #\} #\) *bard-readtable* *default-readtable*)

(set-macro-character 
 #\{
 (lambda (stream ch)(cons 'bard::|map| (read-delimited-list #\} stream)))
 nil *bard-readtable*)

;;; ---------------------------------------------------------------------
;;; token-to-value conversions
;;; ---------------------------------------------------------------------

(defmethod token->value (x) x)

(defmethod token->value ((x null)) nil)

(defmethod token->value ((x (eql 'quote))) 'bard::|quote|)

(defmethod token->value ((x cons)) 
  (cons (token->value (car x))
        (token->value (cdr x))))

(defmethod token->value ((x (eql 'bard::|undefined|))) (%undefined))
(defmethod token->value ((x (eql 'bard::|nothing|))) nil)
(defmethod token->value ((x (eql 'bard::|true|))) (%true))
(defmethod token->value ((x (eql 'bard::|false|))) (%false))
(defmethod token->value ((x (eql 'bard::|eof|))) (%eof))

;;; ---------------------------------------------------------------------
;;; bard read
;;; ---------------------------------------------------------------------

(defun bard-read (&optional (stream *standard-input*)(eof nil))
  (let ((*readtable* *bard-readtable*)
        (*package* (find-package :bard)))
    (token->value (read stream nil eof nil))))

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
;;; (bard-read-convert "/Users/mikel/Workshop/bard/0.5/testdata/literals.bard" "/Users/mikel/Workshop/bard/0.5/testdata/literals.bardo")
