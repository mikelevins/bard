;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          bard.asd
;;;; Project:       Bard
;;;; Purpose:       Bard system definition
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:cl-user)

(asdf:defsystem #:bard
  :serial t
  :depends-on (:folio.as :folio.functions :folio.collections)
  :components ((:file "package")
               (:module "values"
                        :serial t
                        :components ((:file "singletons")
                                     (:file "modules")))
               (:module "reader"
                        :serial t
                        :components ((:file "reader01")
                                     (:file "reader02")))))

;;; (asdf:oos 'asdf:load-op :bard)