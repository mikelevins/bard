;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          set-syntax.lisp
;;;; Project:       folio - the Bard runtime
;;;; Purpose:       a handy literal syntax for sets
;;;; Author:        mikel evins
;;;; Copyright:     2010 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(in-package :set)

;;; =====================================================================
;;; Literal Syntax
;;; =====================================================================

(set-syntax-from-char #\} #\))

(set-dispatch-macro-character #\# #\{
                              (lambda (stream char n)
                                (let ((elts (read-delimited-list #\} stream t)))
                                  `(set:make ,@elts))))

