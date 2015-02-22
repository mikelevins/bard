(begin
  (require "test-load-require-1.scm")

  (define (mytest)
    (format #t "mytest before mytest1~%~!")
    (mytest1)
    (format #t "mytest after mytest1~%~!")))
