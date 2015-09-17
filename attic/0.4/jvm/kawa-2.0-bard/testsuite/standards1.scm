;; Kawa-options: --r7rs %F

(define @ 2)
(define @3 3)
(format #t "five: ~d~%" (+ @ @3))
;; Output: five: 5

(define ten: 10)
(format #t "twenty: ~d~%" (+ ten: ten:))
;; Output: twenty: 20
