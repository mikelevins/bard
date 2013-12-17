;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          compiler1.lisp
;;;; Project:       Bard
;;;; Purpose:       compiler first pass
;;;; Author:        mikel evins
;;;; Copyright:     2013 mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard)

;;; ---------------------------------------------------------------------
;;; 
;;; ---------------------------------------------------------------------

(defparameter *bard-package* (find-package "bard.user"))

(defun current-bard-package () *bard-package*)

(defun comp1-convert-symbol (sym)
  (let* ((package (symbol-package sym))
         (package* (if (bard-package? package)
                       package
                       (current-bard-package))))
    (case sym
      (cl:quote sym)
      (cl:list* sym)
      (t (intern (symbol-name sym) package*)))))

(defun comp1 (exp) 
  (cond
    ((null exp) exp)
    ((numberp exp) exp)
    ((characterp exp) exp)
    ((keywordp exp) exp)
    ((stringp exp) exp)
    ((symbolp exp)(comp1-convert-symbol exp))
    ((listp exp)(cons (comp1 (car exp))
                      (comp1 (cdr exp))))
    (t (error "Syntax error: ~s" exp))))

;;; ---------------------------------------------------------------------
;;; comp1 tests
;;; ---------------------------------------------------------------------

(defun test-comp1 (path)
  (let ((out-path (merge-pathnames (make-pathname :type "bardo1") path)))
    (with-log (out out-path)
      (with-open-file (in path :direction :input)
        (loop for obj = (bard-read in :eof)
           until (eq obj :eof)
           do (let* ((obj* (comp1 obj))
                     (*package* (find-package :keyword)))
                (write obj* :stream out :escape t :readably t)
                (terpri out)))))))

;;; (test-comp1 "/Users/mikel/Workshop/bard/0.5/testdata/literals.bard")
;;; (test-comp1 "/Users/mikel/Workshop/bard/0.5/testdata/namer.bard")
;;; (test-comp1 "/Users/mikel/Workshop/bard/0.5/testdata/programs.bard")

