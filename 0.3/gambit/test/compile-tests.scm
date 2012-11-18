

;;; application
(%compile '(= 2 3) '() #t #f)
(%compile '(+ 2 3 4 5) '() #t #f)

;;; begin
(%compile '(begin 1 2 3) '() #t #f)

;;; constant
(%compile '() '() #t #f)
(%compile 1 '() #t #f)

;;; define variable
(%compile '(define variable x 5) '() #t #f)
(%compile '(define variable x 5) '() #t #t)

;;; if
(%compile '(if #t 'true) '() #t #f)
(%compile '(if #t 'true) '() #t #t)
(%compile '(if (foo? x) 'true 'false) '() #t #t)

;;; method
(%compile '(^ (x) x) '() #t #f)

;;; quote
(%compile '(quote x) '() #t #f)

;;; unless
(%compile '(unless #t 1 2 3) '() #t #f)

;;; when
(%compile '(when #t 1 2 3) '() #t #f)


