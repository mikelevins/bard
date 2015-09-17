(module-export def let)

(import (rename (only (kawa lib std_syntax) let*) (let* let)))
(import (rename (only (kawa standard define) defineRaw) (defineRaw %define)))


(define-syntax def
  (syntax-rules ()
    ((def name val)
     (%define name 0 #!null val))))
