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

(require "OBJC-SUPPORT")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (let* ((load-path (ccl::loading-file-source-file))
         (load-dir (make-pathname :directory (pathname-directory load-path)))) 
    (defun base-directory () load-dir)))

(defparameter $bard-src-files
  '("package"
    "ast"
    ))

(defun compile-and-load (f)
  (with-compilation-unit ()
    (let ((path (make-pathname :name f :defaults (merge-pathnames "src/" (base-directory)))))
      (compile-file path)
      (load path))))

(defun load-bard ()
  (mapcar 'compile-and-load $bard-src-files))



