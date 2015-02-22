;; A runnable module with module-extends and a body that prints something.
(module-extends SimpleB)
(define xx ::integer 20)
(define (get-f) (invoke (this) 'f 5))
(format #t "Hello world - ~d ~a!~%" xx (get-f))
