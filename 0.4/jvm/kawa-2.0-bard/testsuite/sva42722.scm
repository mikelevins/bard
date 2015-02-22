;; Kawa-options: "-f" %F

;; Savannah bug #42722: Explicit type declaration, later instance? check issues
(define v1 ::int 0)
(format #t "is-String? ~w~%" (instance? v1 <String>) )
;; Output: is-String? #f
