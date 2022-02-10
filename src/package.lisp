;;;; package.lisp

(defpackage :bard.internal
  (:use :cl)
  (:shadow))

(defpackage :bard
  (:use :cl :bard.internal))


