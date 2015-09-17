(module-export Bug)

(define-class Bug ()
    (x ::int)
    ((f)
     (receive ((a ::int) (b ::int)) (values 1 2)
         (set! x b))))

((Bug):f)
(define bug (Bug))
(bug:f)
(format #t "bug.x: ~d~%" bug:x)
;; Output: bug.x: 2
