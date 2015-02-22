(module-export mytest1 mytest3)
(require "test-load-require-2.scm")

(define (mytest1)
  (format #t "mytest1 before mytest2~%~!")
  (mytest2)
  (format #t "mytest1 after mytest2~%~!"))
