;; Kawa-options: "-f" %F
(define-class foo () (x1) (x2))
(define-class bar () (y1) (y2))
(define-class foobar (foo bar) (z1) (z2))

(define-constant fb (foobar x2: 22 y1: 31 z2: 49))
(format #t "fb x2:~a z2:~a~%" fb:x2 fb:z2)
;; Output: fb x2:22 z2:49
