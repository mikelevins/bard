#|
 The Great Computer Language Shootout
 http://shootout.alioth.debian.org/

 contributed by Java novice Jarkko Miettinen
 modified ~3 lines of the original C#-version
 by Isaac Gouy
|#

(define (Approximate (n :: int)) :: double
  (let ((u (double[] length: n))  ;; Create unit vector
	(v (double[] length: n))
	(vBv :: double 0)
	(vv :: double 0))
    (do ((i :: int 0 (+ i 1))) ((>= i n)) (set! (u i) 1))
    ;; 20 steps of the power method
    (do ((i :: int 0 (+ i 1))) ((>= i 10))
      (multiplyAtAv n u v)
      (multiplyAtAv n v u))
    ;; B=AtA         A multiplied by A transposed
    ;; v.Bv /(v.v)   eigenvalue of v
    (do ((i :: int 0 (+ i 1))) ((>= i n))
      (let ((vi (v i)))
	(set! vBv (+ vBv (* (u i) vi)))
	(set! vv (+ vv (* vi vi)))))
    (java.lang.Math:sqrt (/ vBv vv))))

;;; return element i,j of infinite matrix A
(define (A (i :: int) (j :: int)) :: double
  (/ 1.0 (+ (quotient (* (+ i j) (+ i j 1)) 2) i 1)))

;;; multiply vector v by matrix A
(define (multiplyAv (n :: int) (v :: double[]) (Av :: double[])) :: void
  (do ((i :: int 0 (+ i 1)))
      ((>= i n))
    (let ((sum :: double 0))
      (do ((j :: int 0 (+ j 1)))
	  ((>= j n))
	(set! sum (+ sum (* (A i j) (v j)))))
      (set! (Av i) sum))))

;;; multiply vector v by matrix A transposed.
(define (multiplyAtv (n :: int) (v :: double[]) (Atv :: double[])) :: void
  (do ((i :: int 0 (+ i 1)))
      ((>= i n))
    (let ((sum :: double 0))
      (do ((j :: int 0 (+ j 1)))
	  ((>= j n))
	(set! sum (+ sum (* (A j i) (v j)))))
      (set! (Atv i) sum))))

;;; multiply vector v by matrix A and then by matrix A transposed.
(define (multiplyAtAv (n :: int) (v :: double[]) (AtAv :: double[])) ::  void
  (let ((u (double[] length: n)))
    (multiplyAv n v u)
    (multiplyAtv n u AtAv)))

(format #t "~,9f~%~!" (Approximate (string->number (cadr (command-line)))))
; java.lang.System:out)
