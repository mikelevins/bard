;;;; ***********************************************************************
;;;;
;;;; Name:          bardvm.asd
;;;; Project:       bard
;;;; Purpose:       bardvm in parenscript
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(require :asdf)

;;; ---------------------------------------------------------------------
;;; bardvm system
;;; ---------------------------------------------------------------------

(asdf:defsystem #:bardvm
  :serial t
  :description "A javacript Bard VM, implemented using parenscript"
  :author "mikel evins <mevins@me.com>"
  :license "Apache 2.0"
  :depends-on (:parenscript)
  :components ((:file "package")
               (:file "bardvm")))

;;; (asdf:load-system :bardvm)
