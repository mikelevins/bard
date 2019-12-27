;;;; package.lisp

(defpackage :bard.internal
  (:use :cl)
  (:shadow #:read #:read-from-string))

(defpackage :bard)
