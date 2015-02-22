(define-syntax something
   (syntax-rules ()
      ((something a b)
       (list a b))))

(define foo
   (lambda (a b)
      (something a b)))
