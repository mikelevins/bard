;;;; ***********************************************************************
;;;;
;;;; Name:          bard.asd
;;;; Project:       the Bard language
;;;; Purpose:       system definition
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(require :asdf)

;;; ---------------------------------------------------------------------
;;; bard system
;;; ---------------------------------------------------------------------

(asdf:defsystem #:bard
    :serial t
    :description "A Lisp"
    :author "mikel evins <mevins@me.com>"
    :license "Apache 2.0"
    :depends-on (:cl-singleton-mixin)
    :components ((:module "src"
                          :serial t
                          :components ((:file "package")
                                       (:file "readtable")
                                       (:file "version")
                                       (:file "singleton-values")
                                       (:file "special")
                                       ))))

;;; (asdf:load-system :bard)
