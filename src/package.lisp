;;;; package.lisp

(defpackage :bard.internal
  (:use :cl :named-readtables)
  (:shadow #:compile))

(defpackage :bard)
