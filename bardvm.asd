;;;; bardvm.asd

(asdf:defsystem #:bardvm
  :description "Describe bardvm here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.5.1"
  :serial t
  :components ((:module "lib"
                        :serial t
                        :components ((:module "paip"
                                              :serial t
                                              :components ((:file "package")
                                                           (:file "auxfns")
                                                           (:file "macros")
                                                           (:file "compiler")
                                                           (:file "optimizers")))))
               (:module "src"
                        :serial t
                        :components ((:file "package")
                                     (:file "bardvm")))))


;;; (asdf:load-system :bardvm)
