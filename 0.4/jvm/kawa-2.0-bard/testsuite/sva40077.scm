(let ((a 0) (b 0.01))
  (format #t "~a~%" (and (eqv? a 0) (not b))))
;; Output: #f

(define (repeat? a b)
  (and (eqv? a 0) (not b)))

(format #t "~a~%" (repeat? 0 0.01))
;; Output: #f
