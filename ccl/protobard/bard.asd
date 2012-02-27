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
               (:file "reader-level-0")
               (:file "reader")
               (:file "runtime")
               (:file "printer")))

;;; (asdf:oos 'asdf:load-op :bard)