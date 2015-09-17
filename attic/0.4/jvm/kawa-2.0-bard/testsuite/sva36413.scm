(module-export foo)

(define (foo) #f)

(define (bar x)
  (let ((y '()))
    (set! y x)
    y))

(format #t "Ok.~%")
;; Diagnostic: sva36413.scm:5:9: warning - no use of bar
;; Output: Ok.
