(define-library (lib1)
  (import (except (scheme base) square))
  (export square)
  (begin
    (define (square n)
      (* n 2))))

(import (scheme base))
(import (prefix (lib1) lib1-))

(display (square 8)) (newline)
(display (lib1-square 8)) (newline)
;; Output: 64
;; Output: 16

