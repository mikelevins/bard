;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          compiler.lisp
;;;; Project:       Bard
;;;; Purpose:       the bard compiler
;;;; Author:        mikel evins
;;;; Copyright:     2013 mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard)

;;; ---------------------------------------------------------------------
;;; comp1
;;; ---------------------------------------------------------------------
;;; the first pass: expand all macros and reader macros. convert all
;;; variable and named constant references to function calls. convert
;;; bard special forms and funcalls to Lisp equivalents and library
;;; calls.

(defun comp1 (exp &optional (env nil)) 
  (cond
    ((named-constant? exp) (comp1-named-constant exp env))
    ((self-evaluating? exp) (comp1-self-evaluating exp env))
    ((symbolp exp) (comp1-variable-reference exp env))
    ((listp exp) (cond
                   ((setter-form? exp)(comp1-setter-form exp env))
                   ((special-form? (car exp))(comp1-special-form exp env))
                   ((macro-form? exp) (comp1 (expand-bard-macro exp) env))
                   (t (comp1-funcall exp env))))
    (t (error "Syntax error: ~s" exp))))

;;; ---------------------------------------------------------------------
;;; file compilation
;;; ---------------------------------------------------------------------
;;; read a source file, run the forms through comp1, and write
;;; the results to a file

;;; comp1-file
;;; ---------------------------------------------------------------------
;;; compile to bardo (Common Lisp sources stored in text form)

(defmethod comp1-file ((in stream)(out stream))
  (loop for obj = (bard-read in (%eof))
     until (%eof? obj)
     do (let ((*package* (find-package :keyword)))
          (write (comp1 obj) :stream out :escape t :readably t)
          (terpri out)))
  (finish-output out))

(defmethod comp1-file ((path pathname) out)
  (with-open-file (in path :direction :input)
    (comp1-file in out)))

(defmethod comp1-file (in (path pathname))
  (with-open-file (out path :direction :output)
    (comp1-file in out)))

(defmethod comp1-file ((path string) out)
  (comp1-file (pathname path) out))

(defmethod comp1-file (in (path string))
  (comp1-file in (pathname path)))

;;; (comp1-file "/Users/mikel/Workshop/bard/0.5/testdata/namer.bard" "/Users/mikel/Workshop/bard/0.5/testdata/namer.bardo")
;;; (comp1-file "/Users/mikel/Workshop/bard/0.5/testdata/literals.bard" "/Users/mikel/Workshop/bard/0.5/testdata/literals.bardo")
;;; (comp1-file "/Users/mikel/Workshop/bard/0.5/testdata/programs.bard" "/Users/mikel/Workshop/bard/0.5/testdata/programs.bardo")

;;; comp1-boxfile
;;; ---------------------------------------------------------------------
;;; compile to box (Common Lisp sources wrapped in a metafile object
;;; and stored in binary form)

(defun comp1-boxfile (path)
  (let* ((out (merge-pathnames (make-pathname :type "box") path))
         (objs (with-open-file (in path :direction :input)
                 (loop for obj = (bard-read in (%eof))
                    until (%eof? obj)
                    collect (let ((*package* (find-package :keyword)))
                              (comp1 obj))))))
    (cl-store:store objs out)))

;;; (comp1-boxfile "/Users/mikel/Workshop/bard/0.5/testdata/programs.bard")
