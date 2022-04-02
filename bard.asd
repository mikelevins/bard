;;;; bard.asd

(ASDF:DEFSYSTEM :bard
  :DESCRIPTION "bard 0.7"
  :AUTHOR "mikel evins <mikel@evins.net>"
  :LICENSE  "specify license here"
  :VERSION "0.7.0"
  :SERIAL t
  :DEPENDS-ON (:NAMED-READTABLES)
  :COMPONENTS ((:MODULE "src"
                :SERIAL T
                :COMPONENTS ((:FILE "package")
                             (:FILE "special")
                             (:FILE "dict")
                             (:FILE "literals")
                             ))))

;;; (ASDF:LOAD-SYSTEM :bard)
