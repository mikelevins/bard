;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          boxes.lisp
;;;; Project:       folio - the Bard runtime
;;;; Purpose:       mutable containers for use with functional collections
;;;; Author:        mikel evins
;;;; Copyright:     2010 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(require :asdf)

(let ((loadpath *load-truename*))
  (defun boxes-root () (make-pathname :directory (pathname-directory loadpath))))

(defpackage "FOLIO.BOXES.SYSTEM" (:use :cl :asdf))

(in-package "FOLIO.BOXES.SYSTEM")

(defsystem folio.boxes
  :serial t
  :depends-on ()
  :components
  ((:file "box")))

(in-package :cl-user)

(defun load-boxes ()
  (asdf:oos 'asdf:load-op :folio.boxes))