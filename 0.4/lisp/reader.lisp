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

(defvar *bard-readtable*
  (let ((tbl (copy-readtable)))
    (setf (readtable-case tbl) :preserve)
    tbl))

;;; ---------------------------------------------------------------------
;;; dispatch-macro characters
;;; ---------------------------------------------------------------------

(set-dispatch-macro-character #\# #\d
  ;; In both Common Lisp and Bard,
  ;; #x, #o and #b are hexidecimal, octal, and binary,
  ;; e.g. #xff = #o377 = #b11111111 = 255
  ;; In Bard only, #d255 is decimal 255.
  #'(lambda (stream &rest ignore)
      (declare (ignore ignore))
      (let ((*read-base* 10)) (bard-read stream)))
  *bard-readtable*)

;;; ---------------------------------------------------------------------
;;; bard reader support
;;; ---------------------------------------------------------------------

(defmethod input->value (x) x)

(defmethod input->value ((x symbol)) 
  (cond
    ((eql x '|nothing|) (nothing))
    ((eql x '|true|) (true))
    ((eql x '|false|) (false))
    (t x)))

;;; ---------------------------------------------------------------------
;;; bard read
;;; ---------------------------------------------------------------------

(defun read-with-bard-module-names (stream eof-error-p eof-value recursive-p)
  (let ((module-name nil))
    (handler-bind 
        ((package-error #'(lambda (err)
                            (setf module-name (slot-value err 'package))
                            (use-value (find-package :bard))))
         (simple-error #'(lambda (err)
                           (let ((restart (find-restart 'continue)))
                             (when restart (invoke-restart restart))))))
      (values (read stream nil (eof) nil)
              module-name))))

(defun bard-read (&optional (stream *standard-input*))
  (let ((*readtable* *bard-readtable*)
        (*package* (find-package :bard)))
    (multiple-value-bind (input mname)(read-with-bard-module-names stream nil (eof) nil)
      (values (input->value input)
              mname))))

(defun bard-read-from-string (s)
  (with-input-from-string (in s)
    (bard-read in)))

