(in-package :bard.internal)
(in-readtable :bard)


;;; (READ-FROM-STRING "2")
;;; (READ-FROM-STRING "(2 3 FOO)")
;;; (READ-FROM-STRING "nothing")
;;; (READ-FROM-STRING ":bar")
;;; (READ-FROM-STRING "foo:bar")
;;; (READ-FROM-STRING "{:a 1 :b 2}")
;;; (READ-FROM-STRING "[:a 1 :b 2]")
