(define (foo)
  (call-with-current-continuation
   (lambda (return)
     (let l ()
       (l)))))
