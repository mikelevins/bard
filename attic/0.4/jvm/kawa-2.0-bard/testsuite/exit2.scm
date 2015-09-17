;; Test that emergency exit doesn't call finally-handler.

(define (fun x)
  (try-finally
   (begin
     (format #t "before exit~%~!")
     (emergency-exit 0)
     (format #t "after exit~%~!"))
   (begin
     (format #t "in-finally~%~!")))
  (format #t "finish fun~%~!")
  (* x x))
(format #t "before call fun~%~!")
(fun 5)
(format #t "after call fun~%~!")

;; Output: before call fun
;; Output: before exit
