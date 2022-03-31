;;;; package.lisp

(defpackage :bard.internal
  (:use :cl :editor-hints.named-readtables)
  (:shadow))

(defpackage :bard
  (:use :cl :bard.internal))


