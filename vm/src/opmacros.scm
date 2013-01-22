
(define-macro (prog1 expr . exprs) `(let ((vv ,expr)) ,@exprs vv))
(define-macro (incpc!) `(set! $pc (+ $pc 1)))

(define-macro (pushv! v) `(set! $vals (cons ,v $vals)))
(define-macro (popv!) `(prog1 (car $vals)(set! $vals (cdr $vals))))
(define-macro (lref i j) `(vector-ref (list-ref $env ,i) ,j))
(define-macro (lset! i j v) `(vector-set! (list-ref $env ,i) ,j ,v))
(define-macro (incpc!) `(set! $pc (+ $pc 1)))

(define-macro (fetch code) `(vector-ref ,code $pc))



