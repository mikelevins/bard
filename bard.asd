;;;; bard.asd

(asdf:defsystem #:bard
  :description "Describe bard here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (:fset :named-readtables)
  :components ((:module "src"
                        :serial t
                        :components ((:file "package")
                                     (:file "data")
                                     (:file "readtable")
                                     (:file "reader")
                                     (:file "bard")))))

;;; (asdf:load-system '#:bard)
