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
               (:file "package")
               (:file "modules")
               (:file "simple-values")
               (:file "reader")))

;;; (asdf:oos 'asdf:load-op :bard)
