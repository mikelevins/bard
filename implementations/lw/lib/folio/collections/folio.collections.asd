;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          collections.lisp
;;;; Project:       folio - the Bard runtime
;;;; Purpose:       a congenial API handling functional sequences and maps
;;;; Author:        mikel evins
;;;; Copyright:     2010 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(require :asdf)

(let ((loadpath *load-truename*))
  (defun collections-root () (make-pathname :directory (pathname-directory loadpath))))

(defpackage "FOLIO.COLLECTIONS.SYSTEM" (:use :cl :asdf))

(in-package "FOLIO.COLLECTIONS.SYSTEM")

(defsystem folio.collections
  :serial t
  :depends-on (:folio.as :fset :folio.functions)
  :components
  ((:file "sets")
   (:file "set-syntax")
   (:file "sequences")
   (:file "sequence-syntax")
   (:file "maps")
   (:file "map-syntax")
   (:file "maps-as-sequences")))

(in-package :cl-user)

(defun load-collections ()
  (asdf:oos 'asdf:load-op :folio.collections))