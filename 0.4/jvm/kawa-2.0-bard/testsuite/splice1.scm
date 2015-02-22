(define xa [3 4 5])
(define oarr (object[] 5 4 3))

(define l1 (list @xa 9 @xa))
(format #t "l1: ~w~%" l1)
;; Output: l1: (3 4 5 9 3 4 5)

(format #t "integer[]-1: ~w ~w~%"  (integer[] 66 @oarr 9 @xa) 99)
;; Output: integer[]-1: [66 5 4 3 9 3 4 5] 99

(format #t "int[]-2: ~w ~w~%"  (int[] 66 @xa 9 @oarr) 99)
;; Output: int[]-2: [66 3 4 5 9 5 4 3] 99

(format #t "sum-xa: ~w~%" (+ @xa))
;; Output: sum-xa: 12

(format #t "sum-oarr: ~w~%" (+ @oarr))
;; Output: sum-oarr: 12

(format #t "sum-xb: ~w~%" (+ 100 @xa @xa 13))
;; Output: sum-xb: 137

; Call static varargs method:
(display (java.lang.String:format "<x: %s y: %s z: %s>" @oarr)) (newline)
;; Output: <x: 5 y: 4 z: 3>

;; Static varargs with non-matching splice:
(define xoarr (object[] "<x: %s y: %s>" @oarr))
(display (gnu.kawa.functions.Format:sprintfToString @xoarr)) (newline)
;; Output: <x: 5 y: 4>

(define-private (fooz x y z)
  (list z y x))
(define (bar x)
  (let ((v [4 x]))
    (vector (fooz @v 6) 123)))
(format #t "~w~%" (bar 10))
;; Output: #((6 10 4) 123)

(define (apply-mod x)
  (gnu.math.IntNum:modulo @x))
(format #t "mod: ~w~%" (apply-mod [12 5]))
;; Output: mod: 2
