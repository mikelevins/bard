;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          standard-readers.lisp
;;;; Project:       Bard
;;;; Purpose:       reader functions used for standard Bard syntax
;;;; Author:        mikel evins
;;;; Copyright:     2010 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard)

(defun read-line-comment (&optional stream eof-error-p eof-value recursive-p)
  (let ((stream (or stream *standard-input*)))
    (cl:read-line stream eof-error-p eof-value recursive-p)
    (values)))

(defun read-digit (&optional stream eof-error-p eof-value recursive-p)
  (let ((stream (or stream *standard-input*))
        (num-parts '(#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\0 #\.))
        (break-chars '(#\( #\) #\{ #\} #\[ #\] #\; #\space #\tab #\newline #\return)))
    (flet ((num-part? (c) (member c num-parts :test #'char=))
           (break-char? (c) (member c break-chars :test #'char=)))
      (let ((chars nil))
        (block accumulating
          (loop 
             (let ((ch (read-char stream eof-error-p eof-value recursive-p)))
               (cond
                 ((or (null ch)
                      (break-char? ch))
                  (return-from accumulating (cl:read-from-string (coerce (reverse chars) 'string))))
                 ((num-part? ch)
                  (setf chars (cons ch chars)))
                 (t (error "Unrecognized number syntax"))))))))))

(defparameter $name-characters
  '(#\! #\@ #\$ #\% #\^ #\& #\* #\_ #\+
    #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\0 #\- #\=
    #\Q #\W #\E #\R #\T #\Y #\U #\I #\O #\P
    #\q #\w #\e #\r #\t #\y #\u #\i #\o #\p
    #\A #\S #\D #\F #\G #\H #\J #\K #\L
    #\a #\s #\d #\f #\g #\h #\j #\k #\l
    #\Z #\X #\C #\V #\B #\N #\M #\< #\> #\? 
    #\z #\x #\c #\v #\b #\n #\m #\. #\/))

(defun %convert-read-name (nm)
  (cond
    ((string= nm "nothing")(make-instance 'nothing-expression))
    ((string= nm "true")(make-instance 'true-expression))
    ((string= nm "false")(make-instance 'false-expression))
    (t (make-instance 'name-expression :name nm))))

(defun read-name (&optional stream eof-error-p eof-value recursive-p)
  (let ((stream (or stream *standard-input*))
        (break-chars '(#\( #\) #\{ #\} #\[ #\] #\; #\space #\tab #\newline #\return)))
    (flet ((name-part? (c) (member c $name-characters :test #'char=))
           (break-char? (c) (member c break-chars :test #'char=)))
      (let ((chars nil))
        (block accumulating
          (loop 
             (let ((ch (read-char stream eof-error-p eof-value recursive-p)))
               (cond
                 ((or (null ch)
                      (break-char? ch))
                  (return-from accumulating (%convert-read-name (coerce (reverse chars) 'string))))
                 ((name-part? ch)
                  (setf chars (cons ch chars)))
                 (t (error "Unrecognized name syntax"))))))))))