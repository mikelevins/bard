(define sb (java.lang.StringBuffer))
(define p1 (promise))
(define p2 (promise))
(define p3 (promise))
(promise-set-alias! p3 p2)
(future
 (begin
   (define pv (force p1))
   (sb:append (format "after force: ~s~%~!" pv))
   (promise-set-value! p2 (* 2 pv))
   pv))
(sb:append (format "main thread~%~!"))
(promise-set-value! p1 123)
(sb:append (format "Done: ~s~%~!" (force p3)))
(display (sb:toString))
;; Output: main thread
;; Output: after force: 123
;; Output: Done: 246
