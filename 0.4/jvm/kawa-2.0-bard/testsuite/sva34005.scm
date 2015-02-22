(define-simple-class myclass ())
(define (bar list) (apply myclass[] list))
(format #t "~a~a~%" "result: " (bar '()))
;; Output: result: []
