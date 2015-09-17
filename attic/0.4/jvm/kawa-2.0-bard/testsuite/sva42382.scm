;; Savannah bug #42382: Assigned but unused argument

(format #t "result=~w~%"
        ((lambda (x)
           (set! x (lambda () 123))
           456)
         #f))

;;Output: result=456
