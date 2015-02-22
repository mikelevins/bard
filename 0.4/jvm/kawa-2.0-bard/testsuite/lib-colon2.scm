(define-library (testlib)
  (export ec-:vector-filter)
  (import (scheme base))
  (begin
    (define (ec-:vector-filter vecs)
      (if (null? vecs)
          '()
          (if (zero? (vector-length (car vecs)))
              (ec-:vector-filter (cdr vecs))
              (cons (car vecs) (ec-:vector-filter (cdr vecs))))))))


(import (scheme base)
        (scheme write)
        (testlib))

(display "ok\n")
;; Output: ok
