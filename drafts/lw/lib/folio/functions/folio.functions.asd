;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          functions.lisp
;;;; Project:       folio - the Bard runtime
;;;; Purpose:       combinators
;;;; Author:        mikel evins
;;;; Copyright:     2010 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(require :asdf)

(let ((loadpath *load-truename*))
  (defun functions-root () (make-pathname :directory (pathname-directory loadpath))))

(defpackage "FOLIO.FUNCTIONS.SYSTEM" (:use :cl :asdf))

(in-package "FOLIO.FUNCTIONS.SYSTEM")

(defsystem folio.functions
  :serial t
  :depends-on ()
  :components
  ((:file "fn")
   (:file "functions")))

(in-package :cl-user)

(defun load-functions ()
  (asdf:oos 'asdf:load-op :folio.functions))