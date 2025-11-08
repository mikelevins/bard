(in-package :cl-user)

(asdf:load-system :bard)

(in-package :bard)

;;; ---------------------------------------------------------------------
;;; compiling expressions
;;; ---------------------------------------------------------------------

(setf $code1 (compiler '(2 3 4)))
(setf $code2 (compiler '(begin (display "some text")(newline) (* 3 4))))

;;; ---------------------------------------------------------------------
;;; testing with bard repl:
;;; ---------------------------------------------------------------------

(bard)

;;; bard expressions
;;; -------------------

'(2 3 4)

(begin (display "some text")(newline) (* 3 4))

((fn (x)(+ 1 x)) 100)

(define $x nothing)
(set! $x 1)

(define (add1 x)(+ x 1))

(add1 3)



(let ((x 10)
      (y 12))
  (+ x y))

(let ((x 10)
      (y 12))
  (newline)
  (if (> x y)
      (begin (display 'yep)
             (newline))
      (begin (display 'nope)
             (newline))))

(let ((x 10)
       (y (+ x 2)))
  (+ x y))

(define mul (fn (x y)(* x y)))

(define div /)
