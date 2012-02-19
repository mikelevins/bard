;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          reader02.lisp
;;;; Project:       Bard
;;;; Purpose:       Bard reader pass 2: convert abstract syntax to concrete Bard values
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

;;; ----------------------------------------------------------------------
;;; Bard's read function
;;; ----------------------------------------------------------------------

(in-package :bard)

(defmethod as-bard-value (x)
  (error "unknown syntax type: ~s" x))

(defmethod read ((in stream))
  (let ((*readtable* bard::+bard-read-table+)
        (*package* (find-package :bard)))
    (syntax-for (cl:read in nil (end-of-file) nil))))

(defmethod read ((in string))
  (with-input-from-string (s in)
	(read s)))

;;; (format t "~%~S" (read "nothing"))
;;; (format t "~%~S" (read "true"))
;;; (format t "~%~S" (read "false"))
;;; (format t "~%~S" (read "0"))
;;; (format t "~%~S" (read "1.2"))
;;; (format t "~%~S" (read "#b101"))
;;; (format t "~%~S" (read "\"Foo bar\""))
;;; (format t "~%~S" (read "Frob"))
;;; (format t "~%~S" (read "foo:bar"))
;;; (format t "~%~S" (read "bard.core:define"))
;;; (format t "~%~S" (read "\\c"))
;;; (format t "~%~S" (read "\\space"))
;;; (format t "~%~S" (read "()"))
;;; (format t "~%~S" (read "(foo)"))
;;; (format t "~%~S" (read "[0 1 2]"))
;;; (format t "~%~S" (read "{}"))
;;; (format t "~%~S" (read "{foo bar}"))
;;; (format t "~%~S" (read "{:name \"Fred\" :friends [:wilma :barney :betty]}"))

