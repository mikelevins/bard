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

(in-package :|bard|)

(defparameter *standard-read-table* (copy-readtable))

(defparameter *bard-read-table*
  (let ((table (copy-readtable)))
    (setf (readtable-case table) :preserve)
    table))

;;; ---------------------------------------------------------------------
;;; list syntax
;;; ---------------------------------------------------------------------

(set-macro-character #\[
                     (lambda (stream char)
                       (let ((elts (read-delimited-list #\] stream t)))
                         ` (cl:list ,@elts)))
                     nil *bard-read-table*)

(set-macro-character #\] (get-macro-character #\)) nil *bard-read-table*)

;;; ---------------------------------------------------------------------
;;; map syntax
;;; ---------------------------------------------------------------------

(set-syntax-from-char #\{ #\(  *bard-read-table* *standard-read-table*)
(set-syntax-from-char #\} #\)  *bard-read-table* *standard-read-table*)

(set-macro-character #\{
                     (lambda (stream char)
                       (let* ((elts (read-delimited-list #\} stream t))
                              (slots (loop for tail on elts by #'cddr collect (list (car tail)(cadr tail)))))
                         ` (fset:wb-map ,@slots)))
                     nil *bard-read-table*)


(in-package :fset)

;;; change to a less visually-cluttered printing style for wb-maps

(defun print-wb-map (map stream level)
  (declare (ignore level))
  (pprint-logical-block (stream nil :prefix "{")
    (do-map (x y map)
      (pprint-pop)
      (write-char #\Space stream)
      (pprint-newline :linear stream)
      (write x :stream stream)
      (write-char #\Space stream)
      (write y :stream stream))
    (format stream " }~:[~;/~:*~S~]" (map-default map))))
