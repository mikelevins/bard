(begin
  (require "test-load-require-3.scm")
  (module-export mytest2 mytest3)

  (define (mytest2)
    (format #t "mytest2 before mytest3~%~!")
    (mytest3)
    (format #t "mytest2 after mytest3~%~!")))

