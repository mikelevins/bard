(module-export top-class)

(begin
  (define (bottom proc) (proc))
)

(define (top proc)
  (bottom (lambda () (proc))))

(define-simple-class top-class ()
  ((frob)
   (top (lambda () #f))))

(format #t "frob: ~a~%" ((make top-class):frob))
;; Output: frob: #f
