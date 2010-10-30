;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          maps.lisp
;;;; Project:       folio - the Bard runtime
;;;; Purpose:       a handy literal syntax for sequences
;;;; Author:        mikel evins
;;;; Copyright:     2010 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(in-package :seq)

;;; =====================================================================
;;; Literal Syntax
;;; =====================================================================

(set-syntax-from-char #\[ #\()
(set-syntax-from-char #\] #\))

(set-macro-character #\[
                (lambda (stream char)
                  (let ((elts (read-delimited-list #\] stream t)))
                    `(seq:make ,@elts))))

