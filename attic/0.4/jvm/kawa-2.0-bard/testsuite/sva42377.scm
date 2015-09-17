(begin
  (define a '#f)
  ((lambda (b)
     (set! b (lambda (c) (cons c c)))
     (set! a b))
   #f)
  (format #t "a7: ~w~%" (a 7)))
;; Output: a7: (7 . 7)
