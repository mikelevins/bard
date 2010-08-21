;;;; -*- Mode: Lisp -*-

;;;; cl-unification.asd --
;;;; ASDF system file.

;;;;===========================================================================
;;;; Simple stuff that should be built in ASDF.

(defpackage "CL-UNIFICATION-SYSTEM" (:use "CL" "ASDF"))

(in-package "CL-UNIFICATION-SYSTEM")

(defclass asdf-system-definition-file (asdf:cl-source-file) ())
(defmethod source-file-type ((c asdf-system-definition-file) (s module)) "asd")


(asdf:defsystem :cl-unification
  :author "Marco Antoniotti"
  :serial t
  :components ((:file "unification-package")
               (:file "variables")
               (:file "substitutions")
               (:file "lambda-list-parsing")
               (:file "templates-hierarchy")
               (:file "unifier")
               (:file "match-block")
               (:file "apply-substitution")
               #+asdf-with-optional-dependencies
               (:module "lib-dependent"
                :pathname "lib-dependent"
                :depends-on ("templates-hierarchy" "unifier")
                :components ((:file "cl-ppcre-template"
                              :depends-on (cl-ppcre))
                             ))
               #-asdf-with-optional-dependencies
               (asdf-system-definition-file
                "cl-unification-lib")
               ))

;;;; end of file -- cl-unification.asd --
