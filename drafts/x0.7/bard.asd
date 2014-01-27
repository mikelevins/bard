;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          bard.asd
;;;; Project:       Bard
;;;; Purpose:       bard  in common lisp
;;;; Author:        mikel evins
;;;; Copyright:     2013 mikel evins
;;;;
;;;; ***********************************************************************

(asdf:defsystem #:bard
  :serial t
  :description "The bard programming language, version 0.4"
  :author "mikel evins <mevins@me.com>"
  :license "Apache 2.0"
  :depends-on (:puri :cl-fad :fset :com.informatimago.common-lisp.lisp-reader :singleton-classes)
  :components ((:module "src"
                        :serial t
                        :components ((:file "package")
                                     (:file "version")
                                     (:file "base-singletons")
                                     (:file "reader")
                                     (:file "repl")))))

;;; (asdf:oos 'asdf:load-op :bard)
;;; (bard::bard)

;;; (bard::build-bard (format nil "/Users/mikel/Desktop/"))
