;;;; ============================================================================
;;;; ASDF System Definition for Bard
;;;; ============================================================================

(defsystem "bard"
  :description "A Common Lisp Virtual Machine for the Bard programming language"
  :version (:read-file-form "version.lisp")
  :author "mikel evins (mikel@evins.net)"
  :license "MIT"
  :depends-on ()
  :serial t
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "vm")
                             (:file "monitor")))))

