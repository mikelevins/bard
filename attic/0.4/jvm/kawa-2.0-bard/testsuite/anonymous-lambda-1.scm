(module-extends <pair>)
(define lst1 (let ((x 1)) (list x (lambda (exp) (* 2 exp)))))
(define lst2 (let ((x 2)) (list x (lambda (exp) (* 3 exp)))))
(format #t "result: ~s ~s~%" ((cadr lst1) (car lst1)) ((cadr lst2) (car lst2)))
;; Output: result: 2 6

