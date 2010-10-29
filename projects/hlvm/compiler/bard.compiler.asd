;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          bard.compiler.asd
;;;; Project:       Bard HLVM
;;;; Purpose:       system definition for the Bard compiler
;;;; Author:        mikel evins
;;;; Copyright:     2010 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(require :asdf)

(let ((loadpath *load-truename*))
  (defun compiler-root () (make-pathname :directory (pathname-directory loadpath))))

(defpackage "BARD.COMPILER.SYSTEM" (:use :cl :asdf))

(in-package "BARD.COMPILER.SYSTEM")

(defsystem bard.compiler
  :serial t
  :depends-on ()
  :components
  ((:file "package")
   (:file "reader")))

(in-package :cl-user)

(defun load-compiler ()
  (asdf:oos 'asdf:load-op :bard.compiler))

;;; (load-compiler)