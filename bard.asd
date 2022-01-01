;;;; bard.asd

(ASDF:DEFSYSTEM :bard
  :DESCRIPTION "Describe bard here"
  :AUTHOR "Your Name <your.name@example.com>"
  :LICENSE  "Specify license here"
  :VERSION "0.0.1"
  :SERIAL t
  :COMPONENTS ((:MODULE "src"
                :SERIAL t
                :COMPONENTS ((:FILE "package")
                             (:FILE "bard")))))

;;; (ASDF:LOAD-SYSTEM :bard)
