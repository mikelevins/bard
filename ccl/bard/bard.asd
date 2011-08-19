;;;; bard.asd

(asdf:defsystem #:bard
  :serial t
  :depends-on (:folio.as :folio.functions :folio.collections)
  :components ((:file "package")
               (:file "singleton")
               (:file "values")))

;;; (asdf:oos 'asdf:load-op :bard)