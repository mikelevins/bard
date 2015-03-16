(ql:quickload :parenscript)

(defpackage #:bardvm
  (:use :cl :parenscript))

(in-package #:bardvm)

(defun makevm (in-path out-path)
  (with-open-file (out out-path :direction :output
                       :if-does-not-exist :create
                       :if-exists :supersede)
    (format out "~a" (ps-compile-file in-path))
    (finish-output)))

;;; (defparameter $src "/usr/local/src/bard/0.4/paren/bardvm.parenscript")
;;; (defparameter $js "/usr/local/src/bard/0.4/paren/bardvm.js")
;;; (makevm $src $js)
