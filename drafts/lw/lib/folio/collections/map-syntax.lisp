;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          maps.lisp
;;;; Project:       folio - the Bard runtime
;;;; Purpose:       a handy literal syntax for maps
;;;; Author:        mikel evins
;;;; Copyright:     2010 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(in-package :map)

;;; =====================================================================
;;; Literal Syntax
;;; =====================================================================

(set-syntax-from-char #\{ #\()
(set-syntax-from-char #\} #\))

(set-macro-character #\{
                (lambda (stream char)
                  (let ((elts (read-delimited-list #\} stream t)))
                    ` (map:make ,@elts))))

