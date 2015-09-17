(define-library (lib2)
  (import (scheme base))
  (export twice)
  (begin
    (define (twice n)
      (* n 2))))
(define-library (lib3)
  (import (scheme base))
  (export thrice)
  (begin
    (define (thrice n)
      (* n 3))))
