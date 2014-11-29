;;;; ***********************************************************************
;;;;
;;;; Name:          bard.asd
;;;; Project:       bard 0.5
;;;; Purpose:       system loader
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(asdf:defsystem #:bard
  :serial t
  :description "Describe bard here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (:com.informatimago.common-lisp.lisp-reader)
  :components ((:module "src"
                        :serial t
                        :components ((:file "package")
                                     (:file "bard")))))

(defun load-bard ()
  (asdf:load-system :bard))

;;; (load-bard)
