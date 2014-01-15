;;;; ***********************************************************************
;;;; File IDENTIFICATION
;;;;
;;;; Name:          bard.asd
;;;; Project:       Bard
;;;; Purpose:       bard implemented in Common Lisp
;;;; Author:        mikel evins
;;;; Copyright:     2013 mikel evins
;;;;
;;;; ***********************************************************************

(asdf:defsystem #:bard
  :serial t
  :description "The bard programming language, version 0.5"
  :author "mikel evins <mevins@me.com>"
  :license "Apache 2.0"
  :depends-on (:cl-store :fset :series :recur :com.informatimago.common-lisp.lisp-reader)
  :components ((:module "src"
                        :serial t
                        :components ((:file "package")
                                     (:file "version")
                                     (:file "values")
                                     (:file "special")
                                     (:file "reader")
                                     ))))

;;; (asdf:oos 'asdf:load-op :bard)
