;;;; bardvm.asd
;;;; Code from Paradigms of Artificial Intelligence Programming
;;;; Copyright (c) 1991 Peter Norvig
;;;; integrated into bardvm by mikel evins, mikel@evins.net, July 2020

(asdf:defsystem #:bardvm
  :description "bardvm"
  :author "mikel evins <mikel@evins.net>"
  :license  "Apache 2.0"
  :version "0.5.1"
  :serial t
  :depends-on (:fset)
  :components ((:module "src"
                        :serial t
                        :components ((:file "package")
                                     (:file "globals")
                                     (:file "auxfns")
                                     (:file "macros")
                                     (:file "method")
                                     (:file "printer")
                                     (:file "prims")
                                     (:file "gen")
                                     (:file "compiler")
                                     (:file "optimizers")
                                     (:file "vm")
                                     (:file "reader")
                                     (:file "toplevel")))))


;;; (asdf:load-system :bardvm)
