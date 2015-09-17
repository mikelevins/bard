(define-simple-class <Simple> (<Object>)
  (input type: <String> init-value: ""))

(define (main)
  (define count 4)
  (let loop ()
    (set! count (- count 1))
    (if (< count 0)
        (exit 0))
    (format #t "count: ~d~%" count)
    (make <Simple> input: "value")
    (loop)))

(main)
;; Output: count: 3
;; Output: count: 2
;; Output: count: 1
;; Output: count: 0
