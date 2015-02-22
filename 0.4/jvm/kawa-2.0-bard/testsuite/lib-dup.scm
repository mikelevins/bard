(define-library (lib2)
  (import (scheme base))
  (export twice)
  (begin
    (define (twice n)
      (* n 2))))
(define-library (lib2)
  (import (scheme base))
  (export thrice)
  (begin
    (define (thrice n)
      (* n 3))))
;; Diagnostic: lib-dup.scm:7:1: duplicate library name lib2
