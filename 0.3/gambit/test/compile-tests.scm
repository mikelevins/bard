

;;; application
(define $bap (%compile '(fx+ 2 3) '() #t #t))
(define $bo (%assemble $bap))
(define $bx (%link $bo))
(define $fn (%makefn code: $bx))
(define $vm (%makevmstate $fn (%null-env) (%bard-globals)))
(%printstate $vm)
(%stepvm $vm)

(define $bap (%compile '(fx+ 2 3 4 5) '() #t #t))
(define $bo (%assemble $bap))
(define $bx (%link $bo))
(define $fn (%makefn code: $bx))
(define $vm (%makevmstate $fn (%null-env) (%bard-globals)))
(%printstate $vm)
(%stepvm $vm)

;;; begin
(%compile '(begin 1 2 3) '() #t #f)

;;; cond
(%compile '(cond (1 1)) '() #t #f)
(%compile '(cond (1 1)(2 2 3)(3 3 4 5)) '() #t #f)

;;; constant
(%compile '() '() #t #f)
(%compile 1 '() #t #f)

;;; define class
(%compile '(define class Ratio) '() #t #f)

;;; define variable
(%compile '(define variable x 5) '() #t #f)
(%compile '(define variable x 5) '() #t #t)

;;; if
(%compile '(if #t 'true) '() #t #f)
(%compile '(if #t 'true) '() #t #t)
(%compile '(if (foo? x) 'true 'false) '() #t #t)

;;; method
(%compile '(^ (x) x) '() #t #f)
(%compile '(^ (x y & more) (list x y)) '() #t #f)

;;; quote
(%compile '(quote x) '() #t #f)

;;; unless
(%compile '(unless #t 1 2 3) '() #t #f)

;;; when
(%compile '(when #t 1 2 3) '() #t #f)


