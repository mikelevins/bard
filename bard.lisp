;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: asdf -*-
;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          Bard.lisp
;;;; Project:       Bard - a modern Lisp
;;;; Purpose:       build the Bard application image
;;;; Author:        mikel evins
;;;; Copyright:     2009 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)
(require "ASDF")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter $base-directory (asdf::determine-system-pathname nil nil)))

(defparameter $bard-src-files
  '("package"
    "utils"
    "singletons"
    "values"
    "lib"
    "reader"
    "modules"
    "printer"
    "environments"
    "compiler"
    ))

(defun compile-and-load (f)
  (with-compilation-unit ()
    (let ((path (make-pathname :name f :defaults (merge-pathnames "src/" $base-directory))))
      (compile-file path)
      (load path))))

(defun load-bard ()
  (load (merge-pathnames "lib/misc-extensions_1.2.0/misc-extensions.asd" $base-directory))
  (load (merge-pathnames "lib/fset_1.2.2/fset.asd" $base-directory))
  (asdf:oos 'asdf:load-op :fset)
  (mapcar 'compile-and-load $bard-src-files))



