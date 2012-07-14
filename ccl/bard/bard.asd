;;;; Bard.asd

(asdf:defsystem #:bard
  :serial t
  :description "bard in common lisp"
  :author "mikel evins <mevins@me.com>"
  :license "MIT"
  :depends-on (:fset)
  :components ((:file "package")
               (:file "values")
               (:file "modules")
               (:file "types")
               (:file "special")
               (:file "repl")))

;;; (asdf:oos 'asdf:load-op :bard)

(defun load-bard ()
  (asdf:oos 'asdf:load-op :bard))

;;; (load-bard)