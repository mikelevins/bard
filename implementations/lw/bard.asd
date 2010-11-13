;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          bard.asd
;;;; Project:       Bard
;;;; Purpose:       the Bard system definition
;;;; Author:        mikel evins
;;;; Copyright:     2010 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(require :asdf)
(require "parsergen")

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

(add-to-asdf "lib/folio/as/")
(add-to-asdf "lib/folio/boxes/")
(add-to-asdf "lib/folio/functions/")
(add-to-asdf "lib/folio/collections/")

;;; ---------------------------------------------------------------------
;;; whether the running lisp is a delivered app
;;; ---------------------------------------------------------------------

(let ((delivered? nil))
  (defun set-delivered (y-or-n)
    (setf delivered? y-or-n))
  (defun delivered? ()
    delivered?))

;;; ---------------------------------------------------------------------
;;; system definition and loader
;;; ---------------------------------------------------------------------

(defpackage #:bard-asd
  (:use :cl :asdf))

(in-package :bard-asd)

(defsystem bard
  :name "bard"
  :version "0.8"
  :author "mikel evins"
  :description "Bard"
  :depends-on (:folio.as :folio.functions :folio.boxes :folio.collections)
  :components ((:module src :serial t
                        :components ((:file "package")
                                     (:file "singleton")
                                     (:file "values")
                                     (:file "reader")))))

(in-package :cl-user)

(defun load-bard ()
  (asdf:oos 'asdf:load-op :bard)
  (init-bard-reader))

;;; (load-bard)
