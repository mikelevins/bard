(define-simple-class A ()
    ((get-some-func) ::int 3))

(define-class B ()
    ((get-some-func) ::int 4)
    ((not-get-func) ::int 5))

(display (A):some-func) (newline)
;; Output: 3
(display ((A):get-some-func)) (newline)
;; Output: 3

(display (B):some-func) (newline)
;; Output: 4
(display ((B):get-some-func)) (newline)
;; Output: 4

(display ((B):not-get-func)) (newline)
;; Output: 5
