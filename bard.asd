;;;; ***********************************************************************
;;;;
;;;; Name:          bard.asd
;;;; Project:       the bard programming language
;;;; Purpose:       system definition
;;;; Author:        mikel evins
;;;; Copyright:     2026 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(require :asdf)

;;; ---------------------------------------------------------------------
;;; bard
;;; ---------------------------------------------------------------------

(asdf:defsystem :bard
  :description "the bard programming language"
  :author "mikel evins <mikel@evins.net>"
  :license "Apache 2.0"
  :version (:read-file-form "version.lisp")
  :serial t
  :depends-on ()
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "types")
                             (:file "opcodes")
                             (:file "vm")
                             (:file "primitives"))))
  :in-order-to ((asdf:test-op (asdf:test-op :bard/test))))

(asdf:defsystem :bard/test
  :description "tests for bard"
  :author "mikel evins <mikel@evins.net>"
  :license "Apache 2.0"
  :version (:read-file-form "version.lisp")
  :serial t
  :depends-on (:bard :fiveam)
  :components ((:module "test"
                :serial t
                :components ((:file "test"))))
  :perform (asdf:test-op (op c)
             (declare (ignore op c))
             (uiop:symbol-call :bard-test :run-tests)))

;;; The runtime system depends on nothing. Tests are isolated in
;;; bard/test so that loading bard does not pull in fiveam.

#+repl (asdf:load-system :bard) ; => T
#+repl (asdf:test-system :bard) ; => T
