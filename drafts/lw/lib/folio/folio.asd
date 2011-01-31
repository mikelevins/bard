;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          folio.lisp
;;;; Project:       folio - the Bard runtime
;;;; Purpose:       system definition for folio
;;;; Author:        mikel evins
;;;; Copyright:     2010 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(require :asdf)

(let ((loadpath *load-truename*))
  (defun folio-root () (make-pathname :directory (pathname-directory loadpath))))

(let* ((sysdefs (directory (merge-pathnames "**/*.asd" (folio-root))))
       (asdf-systems (map 'list
                          (lambda (s) (make-pathname :directory (pathname-directory s)))
                          sysdefs)))
  (dolist (s asdf-systems)
    (pushnew s asdf:*central-registry* :test 'equal)))

(defpackage "FOLIO.SYSTEM" (:use :cl :asdf))

(in-package :folio.system)

(defsystem folio
  :serial t
  :depends-on (:folio.as :folio.functions :folio.boxes :folio.collections))

(in-package :cl-user)

(defun load-folio ()
  (asdf:oos 'asdf:load-op :folio))

;;; (load-folio)