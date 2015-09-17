(define-library (test export)
  (export fifty-five list-4-values test-guard
          make-foo foo? foo-x set-foo-x! foo-y)
  (import (scheme base))
  (begin

    (define-syntax fifty-five
      (syntax-rules ()
        ((_) 55)))

    (define-record-type <foo>
      (make-foo x y)
      foo?
      (x foo-x set-foo-x!)
      (y foo-y))

    (define (list-4-values)
      (let-values (((a b c d)
                    (values 0 1 2 3)))

        (list a b c d)))

    (define (test-guard)
      (guard
       (exn (#t #t))
       'g-5))
    ))

(import (scheme base)
        (scheme write)
        (test export))

(display (fifty-five))
(newline)
;; Output: 55

(let ((bar (make-foo 3 7)))
  (set-foo-x! bar 5)
  (display (foo-x bar))
  (newline))
;; Output: 5

(write (list-4-values)) (newline)
;; Output: (0 1 2 3)

(display (test-guard)) (newline)
;; Output: g-5
