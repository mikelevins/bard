(in-package :cl-user)

(asdf:load-system :bard)

(in-package :bard)

;;; ---------------------------------------------------------------------
;;; compiling expressions
;;; ---------------------------------------------------------------------

(comp-show 3)
(comp-show '(2 3 4))
(setf $code2 (compiler '(begin (display "some text")(newline) (* 3 4))))

;;; ---------------------------------------------------------------------
;;; testing with bard repl:
;;; ---------------------------------------------------------------------

(bard)


;;; bard expressions
;;; -------------------

101

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

;; should return 4
(+ 1 (call/cc (fn (k) (+ 2 (k 3)))))

;; should count to 10 then exit using the captured continuation
(call/cc (fn (exit)
             (let ((count 0))
               (define (testloop)
                   (if (>= count 10)
                       (exit (begin (newline)
                                    (display "called exit")
                                    (newline)
                                    nothing))
                       (begin (newline)
                              (set! count (+ 1 count))
                              (display count)))
                 (testloop))
               (testloop))))
