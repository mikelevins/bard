(define fun1 (cadr fun2))
(define-early-constant fun2
  (list (lambda (x) (+ x 2)) (lambda (x) (+ x 3))))

(display (fun1 10)) (newline)
;; Output: 13
