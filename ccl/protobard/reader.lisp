;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          runtime.lisp
;;;; Project:       Bard
;;;; Purpose:       Bard reader
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:bard)

(defmethod read ((in stream) &optional (eof-value :eof))
  (let ((*readtable* +bard-read-table+)
        (*package* (find-package :bard)))
    (cl:read in nil eof-value)))

(defmethod read ((s string) &optional (eof-value :eof))
  (with-input-from-string (in s)
	(read in eof-value)))

;;; (bard::read "nothing")
;;; (bard::read "foo:bar")
;;; (bard::read "#\\x")
;;; (bard::read "#\\space")
;;; (bard::read "()")
;;; (bard::read "(foo bar baz)")
;;; (bard::read "[]")
;;; (bard::read "[foo bar baz]")
;;; (bard::read "{}")
;;; (bard::read "{name \"Fred\" age 101}")