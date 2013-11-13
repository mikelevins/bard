;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          bard.asd
;;;; Project:       Bard
;;;; Purpose:       the bard compiler and vm in common lisp, derived from Norvig's Scheme
;;;; Author:        mikel evins
;;;; Copyright:     2013 mikel evins
;;;;                Portions copyright 1991 by Peter Norvig
;;;;
;;;; ***********************************************************************

(asdf:defsystem #:bard
  :serial t
  :description "The bard programming language, version 0.4"
  :author "mikel evins <mevins@me.com>"
  :license "Apache 2.0"
  :depends-on (:puri :cl-fad)
  :components ((:module "lib"
                        :serial t
                        :components ((:file "source-form")
                                     (:file "reader")))
               (:module "src"
                        :serial t
                        :components ((:file "package")
                                     (:file "version")
                                     (:file "utils")
                                     (:file "types")
                                     (:file "values-simple")
                                     (:file "environments")
                                     (:file "primitives")
                                     (:file "primitives-alist")
                                     (:file "primitives-arithmetic")
                                     (:file "primitives-boolean")
                                     (:file "primitives-compiler")
                                     (:file "primitives-cons")
                                     (:file "primitives-functions")
                                     (:file "primitives-names")
                                     (:file "primitives-resources")
                                     (:file "primitives-streams")
                                     (:file "primitives-string")
                                     (:file "primitives-types")
                                     (:file "macros")
                                     (:file "reader")
                                     (:file "quasiquote")
                                     (:file "optimizers")
                                     (:file "assembler")
                                     (:file "instructions")
                                     (:file "methods")
                                     (:file "method-tables")
                                     (:file "functions")
                                     (:file "printer")
                                     (:file "compiler")
                                     (:file "vm")
                                     (:file "toplevel")))))

(asdf:defsystem #:bard-test
  :serial t
  :description "Tests of the Bard compiler and VM"
  :author "mikel evins <mevins@me.com>"
  :license "Apache 2.0"
  :depends-on (:lift :bard)
  :components ((:module "test"
                        :serial t
                        :components ((:file "package")
                                     (:file "utils")
                                     (:file "types")
                                     (:file "values-simple")
                                     (:file "environments")
                                     (:file "primitives")
                                     (:file "primitives-alist")
                                     (:file "primitives-arithmetic")
                                     (:file "primitives-boolean")
                                     (:file "primitives-compiler")
                                     (:file "primitives-cons")
                                     (:file "primitives-functions")
                                     (:file "primitives-names")
                                     (:file "primitives-resources")
                                     (:file "primitives-streams")
                                     (:file "primitives-string")
                                     (:file "primitives-types")
                                     (:file "macros")
                                     (:file "reader")
                                     (:file "quasiquote")
                                     (:file "optimizers")
                                     (:file "assembler")
                                     (:file "instructions")
                                     (:file "methods")
                                     (:file "method-tables")
                                     (:file "functions")
                                     (:file "printer")
                                     (:file "compiler")
                                     (:file "vm")))))

;;; (asdf:oos 'asdf:load-op :bard)
;;; (bard::bard)

;;; (bard::build-bard (format nil "/Users/mikel/Desktop/"))

;;; (asdf:oos 'asdf:load-op :bard-test)

;;; compiler
