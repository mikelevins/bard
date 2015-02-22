;; Kawa-options: "--no-inline" "-f" %F
;; Test extracted from test.scm, originally r4rstest.scm by Aubrey Jaffer.

(define add3 (lambda (x) (+ x 3)))
(format #t "add3 3: ~w~%" (add3 3))
;; Output: add3 3: 6
(define first car)
(format #t "first 1 2: ~w~%" (first '(1 2)))
;; Output: first 1 2: 1
(define old-+ +)
(begin (begin (begin)
	      (begin (begin (begin) (define + (lambda (x y) (list y x)))
			    (begin)))
	      (begin))
       (begin)
       (begin (begin (begin) (format #t "add3 6: ~w~%" (add3 6))
		     (begin))))
;; Output: add3 6: (3 6)
(set! + old-+)
(format #t "add3 6: ~w~%" (add3 6))
;; Output: add3 6: 9
