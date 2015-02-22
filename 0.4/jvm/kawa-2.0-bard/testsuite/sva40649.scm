(define (f1 f2) (f2))

(define (fa x)
  (call-with-current-continuation
   (lambda (k)
     (define (f3) x)
     (f1 f3))))

(define (fb x)
  (call-with-current-continuation
   (lambda (k)
     (define (f3) x)
     (f1 f3)
     (+ 10 x))))
(format #t "(fb 3): ~w~%" (fb 3))
;; Output: (fb 3): 13

(define (fc x)
  (call-with-current-continuation
   (lambda (k)
     (define (f3) x)
     (f1 f3)
     (if (> x 0)
         (k (+ 20 x)))
     (+ 10 x))))
(format #t "(fb 3): ~w~%" (fc 3))
;; Output: (fb 3): 23
