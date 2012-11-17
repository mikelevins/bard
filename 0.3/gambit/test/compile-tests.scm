

;;; application
(%compile '(= 2 3) '() #t #f)
(%compile '(+ 2 3 4 5) '() #t #f)

;;; begin
(%compile '(begin 1 2 3) '() #t #f)

;;; constant
(%compile '() '() #t #f)
(%compile 1 '() #t #f)

;;; if
(%compile '(if #t 'true) '() #t #f)
(%compile '(if #t 'true) '() #t #t)
(%compile '(if (foo? x) 'true 'false) '() #t #t)
