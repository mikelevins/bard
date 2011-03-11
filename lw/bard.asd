;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          bard.asd
;;;; Project:       Bard
;;;; Purpose:       system definitions
;;;; Author:        mikel evins
;;;; Copyright:     2011 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(require :asdf)

;;; ---------------------------------------------------------------------
;;; dev-time path utils
;;; ---------------------------------------------------------------------

(let* ((path *load-truename*)
       (project-root (make-pathname :directory (pathname-directory path))))
  ;;; when the app is delivered, we redefine path-base to resolve
  ;;; paths relative to the app bundle
  (defun path-base () project-root))

(defun path (p)(merge-pathnames p (path-base)))

(defun add-to-asdf (path)
  (pushnew (truename (merge-pathnames path (path-base)))
           asdf:*central-registry* :test 'equalp))

;;; ---------------------------------------------------------------------
;;; whether the running lisp is a delivered app
;;; ---------------------------------------------------------------------

(let ((delivered? nil))
  (defun set-delivered (y-or-n)
    (setf delivered? y-or-n))
  (defun delivered? ()
    delivered?))

;;; ---------------------------------------------------------------------
;;; system definitions and loaders
;;; ---------------------------------------------------------------------

(defpackage #:bard-asd
  (:use :cl :asdf))

(in-package :bard-asd)

(require "parsergen")

(defsystem bard
  :name "bard"
  :version "0.9a"
  :author "mikel evins"
  :description "The Bard Programming Language"
  :depends-on (:split-sequence :fset :graylex)
  :serial t
  :components ((:module src :serial t
                        :components
                        ((:file "package")
                         (:file "singleton")
                         (:file "value")
                         (:file "module")
                         (:file "sequence-protocol")
                         (:file "env")
                         (:file "prims")
                         (:file "runtime")
                         (:file "read")
                         (:file "eval")
                         (:file "bard")
                         (:file "main")))))

;;; (asdf::oos 'asdf:load-op :bard)

#| test code
(in-package :bard)


|#
