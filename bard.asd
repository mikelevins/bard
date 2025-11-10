;;;; ***********************************************************************
;;;;
;;;; Name:          bard.asd
;;;; Project:       the bard programming language
;;;; Purpose:       system definition
;;;; Author:        mikel evins
;;;; Copyright:     2024-2025 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(require :asdf)

(asdf:defsystem :bard
  :description "bard 0.9"
  :author "mikel evins <mikel@evins.net>"
  :license  "Apache 2.0"
  :version (:read-file-form "version.lisp")
  :serial t
  :depends-on ()
  :components ((:module "src"
                        :serial t
                        :components ((:file "package")
                                     (:file "utilities")
                                     (:file "macro")
                                     (:file "env")
                                     (:file "bard-macros")
                                     (:file "prim")
                                     (:file "fn")
                                     (:file "compile")
                                     (:file "machine")
                                     ))))

#+repl (asdf:load-system :bard)

