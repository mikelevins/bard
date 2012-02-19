;;;; bard.asd

(asdf:defsystem #:bard
  :serial t
  :components ((:file "package")
               (:module "values"
                        :serial t
                        :components ())
               (:module "reader"
                        :serial t
                        :components ((:file "reader01")
                                     (:file "reader02")))))

;;; (asdf:oos 'asdf:load-op :bard)