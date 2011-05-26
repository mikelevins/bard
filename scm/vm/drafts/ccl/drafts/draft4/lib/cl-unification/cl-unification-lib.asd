;;;; -*- Mode: Lisp -*-

;;;; cl-unification-lib.asd --
;;;; ASDF system file.

(asdf:defsystem :cl-unification-lib
  :author "Marco Antoniotti"
  :components ((:module "lib-dependent"
                :components ((:file "cl-ppcre-template"))))
  :depends-on (cl-ppcre cl-unification))

;;;; end of file -- cl-unification-lib.asd --
