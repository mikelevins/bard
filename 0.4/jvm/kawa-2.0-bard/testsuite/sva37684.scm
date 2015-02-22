(define-alias C java.lang.Character)

(define c1 ::C (C:valueOf #\X))
(define c2 ::C:Subset  C:UnicodeBlock:ARABIC)

(define-simple-class TestNestedType ()
    (x1 ::C)
    (x2 ::C:Subset))

(define tn ::TestNestedType (TestNestedType x1: c1 x2: c2))
(format #t "x1: ~a x2: ~a~%~!" tn:x1 tn:x2)
;; Output: x1: X x2: ARABIC
