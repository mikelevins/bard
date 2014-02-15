;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          reader.lisp
;;;; Project:       Bard
;;;; Purpose:       bard reader
;;;; Author:        mikel evins
;;;; Copyright:     2014 mikel evins
;;;;                Incrementally derived from Peter Norvig's Scheme compiler
;;;;                Code from Paradigms of Artificial Intelligence Programming
;;;;                Copyright (c) 1991 Peter Norvig
;;;;
;;;; ***********************************************************************

(in-package :bard)


;;; ==============================

(defconstant eof "EoF")
(defun eof-object? (x) (eq x eof))
(defvar *bard-readtable* (copy-readtable))

(set-macro-character #\` 
                     #'(lambda (s ignore)(declare (ignore ignore)) (list 'quasiquote (bard-read s))) 
                     nil *bard-readtable*)

(set-macro-character #\, 
                     #'(lambda (stream ignore)
                         (declare (ignore ignore))
                         (let ((ch (read-char stream)))
                           (if (char= ch #\@)
                               (list 'unquote-splicing (read stream))
                               (progn (unread-char ch stream)
                                      (list 'unquote (read stream))))))
                     nil *bard-readtable*)

;;; ==============================

(defun bard-read (&optional (stream *standard-input*))
  (let ((*readtable* *bard-readtable*))
    (convert-numbers (read stream nil eof))))

(defun convert-numbers (x)
  "Replace symbols that look like Bard numbers with their values."
  ;; Don't copy structure, make changes in place.
  (typecase x
    (cons   (setf (car x) (convert-numbers (car x)))
            (setf (cdr x) (convert-numbers (cdr x)))
	    x) ; *** Bug fix, gat, 11/9/92
    (cl:symbol (or (convert-number x) x))
    (vector (dotimes (i (length x))
              (setf (aref x i) (convert-numbers (aref x i))))
	    x) ; *** Bug fix, gat, 11/9/92
    (t x)))

(defun convert-number (symbol)
  "If str looks like a complex number, return the number."
  (let* ((str (symbol-name symbol))
         (pos (position-if #'sign-p str))
         (end (- (length str) 1)))
    (when (and pos (char-equal (char str end) #\i))
      (let ((re (read-from-string str nil nil :start 0 :end pos))
            (im (read-from-string str nil nil :start pos :end end)))
        (when (and (numberp re) (numberp im))
          (complex re im))))))

(defun sign-p (char) (find char "+-"))
