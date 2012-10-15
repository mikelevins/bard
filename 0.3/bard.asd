;;;; bard.asd

(asdf:defsystem #:bard
  :serial t
  :description "the bard programming language"
  :author "mikel evins <mevins@me.com>"
  :license "Apache 2.0 license"
  :depends-on (:parenscript)
  :components ((:file "package")
               (:file "bard")))

;;; (asdf:oos 'asdf:load-op :bard)
