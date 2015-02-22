;; Kawa-options: "--no-inline" "-f" %F
;; Based on Savannah bug #43131
;;   "Poor support for re-defining toplevel variables", by Helmut Eller.

(define (foo) 1)
(define (bar i) (format #t "version ~d: ~s~%" i (foo)) (force-output))
(bar 1)
;; Output: version 1: 1
(define (foo) 2)
(bar 2)
;; Output: version 2: 2
(define foo (lambda () 3))
(bar 3)
;; Output: version 3: 3
(define (foo) 4)
(bar 4)
;; Output: version 4: 4
(define (foo) 5)
(bar 5)
;; Output: version 5: 5
