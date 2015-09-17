(define-syntax macro1
  (syntax-rules ()
    ((_ f1 ignored)
     (define f1
       (call-with-values
           (lambda ()
             (define (f1 f2)
               (f2))
             (values f1))
         (lambda (x) x))))))

(define-syntax macro2
  (syntax-rules ()
    ((_ f5)
     (let ()
       (macro1 f1 (lambda (x) (f5)))
       (define (f3)
         (define (f4) #f)
         (f1 f4))
       f3))))

(define f (macro2 (lambda () #f)))
(format #t "f: ~w~%" f)
;; Output: f: #<procedure f3>
