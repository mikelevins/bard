(define-syntax be-like-begin
  (syntax-rules ()
    ((be-like-begin name)
     (define-syntax name
       (syntax-rules ()
	 ((name expr (... ...))
	  (begin expr (... ...))))))))
(be-like-begin sequence)
(display (sequence 1 2 3 4))
(newline)
;; Output: 4
