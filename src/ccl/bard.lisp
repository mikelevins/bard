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

(require :asdf)

(let ((fpath *load-truename*))
  (defun bard-root () (make-pathname :directory (pathname-directory fpath))))

(defparameter $bard-src-files
  '(
    "src/packages"
    "src/singleton"
    "src/values"
    "src/cells"
    "src/names"
    "src/sequences"
    "src/maps"
    "src/modules"
    "src/protocols"
    #|
    "src/reader"
    "src/printer"
    "src/test-suite"
    "utils"
    "environments"
    "toplevel"
    "apply"
    "functions"
    "compiler"
    |#
    ))

(defun compile-and-load (f)
  (with-compilation-unit ()
    (let ((path (merge-pathnames f (bard-root))))
      (compile-file path)
      (load path))))

(defun asdf-load (sys)
  (funcall (intern "OOS" (find-package "ASDF")) 
           (intern "LOAD-OP" (find-package "ASDF"))
           sys))

(defun load-bard ()
  (load (merge-pathnames "lib/misc-extensions_1.2.0/misc-extensions.asd" (bard-root)))
  (asdf-load 'misc-extensions)
  (load (merge-pathnames "lib/fset_1.2.2/fset.asd" (bard-root)))
  (asdf-load 'FSet)
  (load (merge-pathnames "lib/cl-unification/cl-unification.asd" (bard-root)))
  (asdf-load 'cl-unification)
  (mapcar 'compile-and-load $bard-src-files))


#|

(load-bard)

|#