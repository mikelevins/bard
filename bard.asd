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
                                     (:file "macro")
                                     (:file "env")
                                     (:file "bard-macros")
                                     (:file "prim")
                                     (:file "fn")
                                     (:file "optimize")
                                     (:file "compile")
                                     (:file "optimizers")
                                     (:file "machine")
                                     ))))

#+repl (asdf:load-system :bard)
