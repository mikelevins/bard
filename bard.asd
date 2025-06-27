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
  :description "bard 0.8"
  :author "mikel evins <mikel@evins.net>"
  :license  "Apache 2.0"
  :version (:read-file-form "version.lisp")
  :serial t
  :depends-on ()
  :components ((:module "src"
                        :serial t
                        :components ((:file "package")
                                     (:file "auxfns")
                                     (:file "interp1")
                                     (:file "compile1")
                                     (:file "compile2")
                                     (:file "compile3")
                                     (:file "compopt")))))

#+repl (asdf:load-system :bard)
