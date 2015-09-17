(define (\1- x) (invoke-static 'gnu.jemacs.lang.AddOp '$Mn x 1))

(define (\1+ x) (invoke-static 'gnu.jemacs.lang.AddOp '$Pl x 1))

(define (% x y)
  (invoke-static 'integer 'remainder x y))
