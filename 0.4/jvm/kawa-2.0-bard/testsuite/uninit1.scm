(module-compile-options warn-as-error: #t)
(define (f1 x)
  (define y z)
  (define z (+ x 3))
  (list y z))
;; Diagnostic: uninit1.scm:3:13: variable 'z' may be uninitialized here
(define (f2 x)
  (define x (apply (lambda () (* 2 y))))
  (define y 10)
  (list x y))
;; Diagnostic: uninit1.scm:8:36: variable 'y' may be uninitialized here
(define (f3 x)
  (define (y2) (* 2 y))
  (define y 10)
  (define x2 (y2))
  (list x2 y))
; No warning for f3!
(define (f4 x)
  (define (y2) (* 2 y))
  (define x1 (y2))
  (define y 10)
  (define x2 (y2))
  (list x1 x2 y))
;; Diagnostic: uninit1.scm:19:21: variable 'y' may be uninitialized here
;; Diagnostic: uninit1.scm:20:14: - because of possible call here of function y2
