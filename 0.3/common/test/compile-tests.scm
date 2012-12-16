

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
(define $bap (%compile '(begin 1 2 3) '() #t #t))
(define $bo (%assemble $bap))
(define $bx (%link $bo))
(define $fn (%makefn code: $bx))
(define $vm (%makevmstate $fn (%null-env) (%bard-globals)))
(%printstate $vm)
(%stepvm $vm)

;;; cond
(define $bap (%compile '(cond (1 1)) '() #t #t))
(define $bo (%assemble $bap))
(define $bx (%link $bo))
(define $fn (%makefn code: $bx))
(define $vm (%makevmstate $fn (%null-env) (%bard-globals)))
(%printstate $vm)
(begin
  (%stepvm $vm)
  (%printstate $vm))

(define $bap (%compile '(cond (1 1)(2 2 3)(3 3 4 5)) '() #t #t))
(define $bo (%assemble $bap))
(define $bx (%link $bo))
(define $fn (%makefn code: $bx))
(define $vm (%makevmstate $fn (%null-env) (%bard-globals)))
(%printstate $vm)
(begin
  (%stepvm $vm)
  (%printstate $vm))

;;; constant
(define $bap (%compile 1 '() #t #t))
(define $bo (%assemble $bap))
(define $bx (%link $bo))
(define $fn (%makefn code: $bx))
(define $vm (%makevmstate $fn (%null-env) (%bard-globals)))
(%printstate $vm)
(begin
  (%stepvm $vm)
  (%printstate $vm))

;;; define class
(define $bap (%compile '(define class Ratio) '() #t #t))
(define $bo (%assemble $bap))
(define $bx (%link $bo))
(define $fn (%makefn code: $bx))
(define $vm (%makevmstate $fn (%null-env) (%bard-globals)))
(%printstate $vm)
(begin
  (%stepvm $vm)
  (%printstate $vm))

;;; define variable
(define $bap (%compile '(define variable x 5 mutable: #t) '() #t #t))
(define $bo (%assemble $bap))
(define $bx (%link $bo))
(define $fn (%makefn code: $bx))
(define $vm (%makevmstate $fn (%null-env) (%bard-globals)))
(%printstate $vm)
(begin
  (%stepvm $vm)
  (%printstate $vm))

;;; if
(define $bap (%compile '(if #t 'true) '() #t #t))
(define $bo (%assemble $bap))
(define $bx (%link $bo))
(define $fn (%makefn code: $bx))
(define $vm (%makevmstate $fn (%null-env) (%bard-globals)))
(%printstate $vm)
(begin
  (%stepvm $vm)
  (%printstate $vm))

(define $bap (%compile '(if #f 'false) '() #t #t))
(define $bo (%assemble $bap))
(define $bx (%link $bo))
(define $fn (%makefn code: $bx))
(define $vm (%makevmstate $fn (%null-env) (%bard-globals)))
(%printstate $vm)
(begin
  (%stepvm $vm)
  (%printstate $vm))

;;; let
(define $bap (%compile ' (let () 1)'() #t #t))
(define $bap (%compile ' (let ((x 1)) x)'() #t #t))
(define $bap (%compile ' (let ((x 1)(y 2)(z 3)) z)'() #t #t))
(define $bap (%compile ' (let ((x y z (values 1 2 3))) z)'() #t #t))



;;; method
(define $bap (%compile '(^ (x) x) '() #t #T))
(define $bo (%assemble $bap))
(define $bx (%link $bo))
(define $fn (%makefn code: $bx))
(define $vm (%makevmstate $fn (%null-env) (%bard-globals)))
(%printstate $vm)
(begin
  (%stepvm $vm)
  (%printstate $vm))

(define $bap (%compile '(^ (x y) (fx+ x y)) '() #t #t))
(define $bo (%assemble $bap))
(define $bx (%link $bo))
(define $fn (%makefn code: $bx))
(define $vm (%makevmstate $fn (%null-env) (%bard-globals)))
(%printstate $vm)
(begin
  (%stepvm $vm)
  (%printstate $vm))

;;; quote
(define $bap (%compile '(quote x) '() #t #t))
(define $bo (%assemble $bap))
(define $bx (%link $bo))
(define $fn (%makefn code: $bx))
(define $vm (%makevmstate $fn (%null-env) (%bard-globals)))
(%printstate $vm)
(begin
  (%stepvm $vm)
  (%printstate $vm))

;;; unless
(define $bap (%compile '(unless #t 1 2 3) '() #t #t))
(define $bo (%assemble $bap))
(define $bx (%link $bo))
(define $fn (%makefn code: $bx))
(define $vm (%makevmstate $fn (%null-env) (%bard-globals)))
(%printstate $vm)
(begin
  (%stepvm $vm)
  (%printstate $vm))

;;; when
(define $bap (%compile '(when #t 1 2 3) '() #t #t))
(define $bo (%assemble $bap))
(define $bx (%link $bo))
(define $fn (%makefn code: $bx))
(define $vm (%makevmstate $fn (%null-env) (%bard-globals)))
(%printstate $vm)
(begin
  (%stepvm $vm)
  (%printstate $vm))


