;;;; bard.asd

(asdf:defsystem #:bard
  :serial t
  :description "the bard programming language"
  :author "mikel evins <mevins@me.com>"
  :license "Apache 2.0 License"
  :components ((:file "package")
               (:file "reader")
               (:file "printer")
               (:file "values")))


