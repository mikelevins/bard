(define-library (test colon)
  (export okay:another)
  ;; Specifically don't include quote/quasiquote
  (import (only (scheme base) define))
  (begin
    (define (okay:another) 8)))

(import (test colon))

(define (okay:something) 7)
(display (okay:something))
(newline)
;; Output: 7
(display (okay:another))
(newline)
;; Output: 8
