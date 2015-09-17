(define-class A ()
  ((some-method (x ::int) (f ::procedure)) ::int
   (f x)))

(define-class B ()
  (p ::int 42)
  ((another-method) ::int
   ((A):some-method 13 (lambda (o ::int) ::int (+ o p)))))

(display ((B):another-method)) (newline)
;; Output: 55
