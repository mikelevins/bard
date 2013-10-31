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
  :depends-on (:puri)
  :components ((:module "lib"
                        :serial t
                        :components ((:file "source-form")
                                     (:file "reader")))
               (:module "src"
                        :serial t
                        :components ((:file "package")
                                     (:file "version")
                                     (:file "utils")
                                     (:file "values-simple")
                                     (:file "globals")
                                     (:file "environments")
                                     (:file "primitives")
                                     (:file "primitives-alist-map")
                                     (:file "primitives-arithmetic")
                                     (:file "primitives-boolean")
                                     (:file "primitives-compiler")
                                     (:file "primitives-cons")
                                     (:file "primitives-names")
                                     (:file "primitives-resources")
                                     (:file "primitives-streams")
                                     (:file "primitives-string")
                                     (:file "macros")
                                     (:file "reader")
                                     (:file "quasiquote")
                                     (:file "optimizers")
                                     (:file "assembler")
                                     (:file "instructions")
                                     (:file "methods")
                                     (:file "printer")
                                     (:file "compiler")
                                     (:file "vm")
                                     (:file "toplevel")))))

;;; (asdf:oos 'asdf:load-op :bard)
;;; (bard::bard)

;;; (bard::build-bard (format nil "/Users/mikel/Desktop/"))
