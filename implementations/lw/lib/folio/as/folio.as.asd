;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          as.asd
;;;; Project:       folio - the Bard runtime
;;;; Purpose:       general type conversions
;;;; Author:        mikel evins
;;;; Copyright:     2010 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(require :asdf)

(let ((loadpath *load-truename*))
  (defun as-root () (make-pathname :directory (pathname-directory loadpath))))

(defpackage "FOLIO.AS.SYSTEM" (:use :cl :asdf))

(in-package "FOLIO.AS.SYSTEM")

(defsystem folio.as
  :serial t
  :components
  ((:file "as")))

(in-package :cl-user)

(defun load-as ()
  (asdf:oos 'asdf:load-op :folio.as))