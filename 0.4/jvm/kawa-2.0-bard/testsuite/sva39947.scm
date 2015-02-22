(define (hash-addall-test x::java.util.HashSet y::java.util.HashSet)
  (x:addAll y))

(let ((x (java.util.LinkedHashSet)) (y (java.util.LinkedHashSet)))
  (y:add "foo")
  (y:add "bar")
  (hash-addall-test x y)
  (format #t "x: ~a~%~!" x))
;; Output: x: [foo, bar]
