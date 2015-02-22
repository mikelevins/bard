(module-name <cycle2>)
(module-export is-odd? c2x double-c1x)

(require <cycle1>)

(define c2x 3) 
(double-c2x) 
(define (double-c1x)
  (set! c1x (* 2 c1x)))

(define one :: <int> 0)
; Verifies that body is executed exactly once.
(set! one (+ one 1))

(define (is-odd? (x :: <int>)) :: <boolean>
  (if (= x 0) #f (is-even? (- x one))))
