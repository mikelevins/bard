(in-package :cl-user)

(asdf:load-system :bard)

(in-package :bard)

(bard)
;; test in bard repl:
((fn (x)(+ 1 x)) 100)
(define (add1 x)(+ x 1))
(add1 3)
(let ((x 10)
      (y 12))
  (+ x y))
;; fails:
(let* ((x 10)
       (y (+ x 2)))
  (+ x y))
(define mul (fn (x y)(* x y)))
(define div /)
