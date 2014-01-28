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
    :depends-on (:puri :cl-fad :fset :singleton-classes)
    :components ((:module "src"
                          :serial t
                          :components ((:file "package")
                                       (:file "version")
                                       (:file "base-singletons")
                                       (:file "env")
                                       (:file "compiler1")
                                       (:file "special-forms")))))

;;; (asdf:load-system :bard)

