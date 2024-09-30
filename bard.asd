;;;; ***********************************************************************
;;;;
;;;; Name:          bard.asd
;;;; Project:       the bard programming language
;;;; Purpose:       system definition
;;;; Author:        mikel evins
;;;; Copyright:     2024 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(require :asdf)

(asdf:defsystem :bard
  :description "bard 0.7"
  :author "mikel evins <mikel@evins.net>"
  :license  "specify license here"
  :version (:read-file-form "version.lisp")
  :serial t
  :depends-on (:eclector)
  :components ((:module "src"
                        :serial t
                        :components ((:file "package")
                                     (:file "base-data")
                                     (:file "reader")
                                     (:file "printer")))))

#+repl (asdf:load-system :bard)
