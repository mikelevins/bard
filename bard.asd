;;;; bard.asd

(asdf:defsystem :bard
  :description "bard 0.7"
  :author "mikel evins <mikel@evins.net>"
  :license  "specify license here"
  :version "0.7.0"
  :serial t
  :depends-on (:named-readtables)
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "special")
                             (:file "dict")
                             (:file "literals")))))

;;; (asdf:load-system :bard)
