;;;; bardvm.asd

(asdf:defsystem #:bardvm
  :description "Describe bardvm here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.5.1"
  :serial t
  :components ((:module "src"
                        :serial t
                        :components ((:file "package")
                                     (:file "auxfns")
                                     (:file "macros")
                                     (:file "compiler")
                                     (:file "optimizers")))))


;;; (asdf:load-system :bardvm)