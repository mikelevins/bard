;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          read.lisp
;;;; Project:       Bard
;;;; Purpose:       the Bard reader
;;;; Author:        mikel evins
;;;; Copyright:     2010 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard)

(defun read (&optional stream eof-error-p eof-value recursive-p)
  (funcall (reader-for (peek-char t stream eof-error-p eof-value recursive-p)
                       *bard-read-table*)
           stream eof-error-p eof-value recursive-p))