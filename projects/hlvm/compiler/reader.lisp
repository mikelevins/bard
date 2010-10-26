;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          reader.lisp
;;;; Project:       Bard HLVM
;;;; Purpose:       the Bard reader
;;;; Author:        mikel evins
;;;; Copyright:     2010 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(defpackage "BARD.READER"
  (:use :cl)
  (:nicknames "BR")
  (:shadow "READ" "READ-TABLE" "*READTABLE*")
  (:export "READ" "*READTABLE*"))

(in-package :BR)

;;; =====================================================================
;;; reader
;;; =====================================================================

;;; ---------------------------------------------------------------------
;;; standard reader functions

(defun read-line-comment (stream eof-error-p eof-value recursive-p)
  (block skipping
    (loop 
       (let ((ch (read-char stream eof-error-p eof-value recursive-p)))
         (when (or (equal ch eof-value)
                   (char= ch #\newline)
                   (char= ch #\return))
           (return-from skipping (values)))))))

;;; ---------------------------------------------------------------------
;;; the standard readtable

(defclass read-table ()
  ())

(defmethod define-reader ((tbl read-table) &key class reader)
  )

(defun make-standard-readtable ()
  (let ((rtbl (make-instance 'read-table)))
    (define-reader rtbl :class :line-comment :reader #'read-line-comment)
    rtbl))

(defparameter *readtable* (make-standard-readtable))

;;; ---------------------------------------------------------------------
;;; read

(defun read (&optional stream eof-error-p eof-value recursive-p)
  (let* ((stream (or stream *standard-input*))
         (ch (peek-char t stream eof-error-p eof-value recursive-p))
         (reader (get-reader *readtable* ch)))
    (funcall reader stream eof-error-p eof-value recursive-p)))

#|
(with-input-from-string (in "  ;;;;")
  (read in))
|#