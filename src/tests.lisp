;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: asdf -*-
;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          tests.lisp
;;;; Project:       Bard - a modern Lisp
;;;; Purpose:       library APIs for Bard base types
;;;; Author:        mikel evins
;;;; Copyright:     2009 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard)

;;; ============================================================
;;; testing
;;; ============================================================

(defun read-test (s)
  (let ((x (read s)))
    (format t "~%~a" x)))

(defparameter $test-expressions
  '(
    ;; text
    "\"foo bar\""
    ;; numbers
    "123.45" "1" "#b101"
    ;; characters
    "\\c" "\\1" "\\space"
    ;; boolean and void
    "true" "false" "void"
    ;; symbols
    "foo" "bard.lang/foo"
    ;; keywords
    ":foo" "foo:" ":foo:"
    ;; sequences
    "()" "(foo)" "(+ (- 5 2)(- 4 3))"
    ;; maps
    "{}" "{name: foo}" "{first: {a b} second: {c d}}"
    ))

(defun run-read-test ()
  (terpri)
  (dolist (s $test-expressions)
    (read-test s))
  (terpri))
