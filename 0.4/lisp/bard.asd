;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          bard.asd
;;;; Project:       Bard
;;;; Purpose:       the bard compiler and vm in common lisp, derived from Norvig's Scheme
;;;; Author:        mikel evins, from Norvig
;;;; Copyright:     1991 by Peter Norvig, 2013 mikel evins
;;;;
;;;; ***********************************************************************

(asdf:defsystem #:bard
  :serial t
  :description "The bard programming language, version 0.4"
  :author "mikel evins <mevins@me.com>"
  :license "Apache 2.0"
  :components ((:module "lib"
                        :serial t
                        :components ((:file "source-form")
                                     (:file "reader")))
               (:module "src"
                        :serial t
                        :components ((:file "package")
                                     (:file "utils")
                                     (:file "values-simple")
                                     (:file "values-maps")
                                     (:file "globals")
                                     (:file "environments")
                                     (:file "modules")
                                     (:file "primitives")
                                     (:file "macros")
                                     (:file "reader")
                                     (:file "quasiquote")
                                     (:file "optimizers")
                                     (:file "assembler")
                                     (:file "instructions")
                                     (:file "methods")
                                     (:file "compiler")
                                     (:file "vm")
                                     ;;(:file "lispvm") ; compile bard programs to lisp and execute them instead o interpreting with a VM
                                     (:file "toplevel")))))

;;; (asdf:oos 'asdf:load-op :bard)

