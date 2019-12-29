(in-package :bard.internal)
(in-readtable :bard)


;;; (READ-FROM-STRING "2")
;;; (READ-FROM-STRING "(2 3 FOO)")
;;; (READ-FROM-STRING "nothing")
;;; (READ-FROM-STRING ":bar")
;;; (READ-FROM-STRING "foo:bar")
;;; (READ-FROM-STRING "{:a 1 :b 2 {:c 3 :d 4}}")
;;; (READ-FROM-STRING "[:a 1 :b 2 [+ 2 3]]")
;;; (READ-FROM-STRING "(a b [1 2 3] [4 5 {:a 1 :b 2 :c [4 3 2]}])")
