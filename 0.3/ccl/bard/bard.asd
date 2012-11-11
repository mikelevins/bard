;;;; bard.asd

(asdf:defsystem #:bard
  :serial t
  :description "Describe bard here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (:fset)
  :components ((:file "package")
               (:file "bard")))

;;; (asdf:oos 'asdf:load-op :bard)
