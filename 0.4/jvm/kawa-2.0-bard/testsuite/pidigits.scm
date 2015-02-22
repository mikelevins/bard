(module-compile-options warn-undefined-variable: #t
			warn-invoke-unknown-method: #t)
;; Based on this Java version:
;; http://shootout.alioth.debian.org/u32q/benchmark.php?test=pidigits&lang=javaxint&id=1
;;  contributed by Isaac Gouy

(define-class Transformation ()
  (q :: integer)
  (r :: integer)
  (s :: integer)
  (t :: integer)
  (k :: int init: 0)

  ((next) :: void
   (set! k (+ k 1))
   (set! q k)
   (set! r (+ (* 4 k) 2))
   (set! s 0)
   (set! t (+ (* 2 k) 1)))

  ((extract (j :: int)) :: int
   (let ((numerator :: integer (+ (* q j) r))
	 (denominator :: integer (+ (* s j) t)))
     ((integer:quotient numerator denominator):intValue)))

  ((qrst (qn :: int) (rn :: int) (sn :: int) (tn :: int)) :: void
   (set! q qn)
   (set! r rn)
   (set! s sn)
   (set! t tn)
   (set! k 0))

  ((compose (a :: Transformation)) :: Transformation
   (Transformation q: (* q a:q)
		   r: (+ (* q a:r) (* r a:t))
		   s: (+ (* s a:q) (* t a:s))
		   t: (+ (* s a:r) (* t a:t)))))

(define-class PiDigitSpigot ()
  (z :: Transformation init: (Transformation q: 1 r: 0 s: 0 t: 1))
  (x :: Transformation init: (Transformation q: 0 r: 0 s: 0 t: 0))
  (inverse :: Transformation init: (Transformation q: 0 r: 0 s: 0 t: 0))

  ((next) :: int
   (let ((y (digit)))
     (if (isSafe y)
	 (begin (set! z (produce y)) y)
	 (begin (x:next) (set! z (consume x)) (next)))))

  ((digit) :: int (z:extract 3))

  ((isSafe (digit :: int)) :: boolean
   (= digit (z:extract 4)))

  ((produce (i :: int)) :: Transformation
   (inverse:qrst 10 (* i -10) 0 1)
   (inverse:compose z))

  ((consume (a :: Transformation)) :: Transformation
   (z:compose a)))

(define-constant L :: int 10)

(define (pidigits (n :: int) (out :: java.io.PrintStream)) :: void
  (let ((j :: int 0)
	(digits (PiDigitSpigot)))
    (do ((n :: int n (- n L)))
	((<= n 0))
      (if (>= n L)
	  (do ((i :: int 0 (+ i 1)))
	      ((>= i L) (set! j (+ j L)))
	    (out:print (digits:next)))
	  (begin
	    (do ((i :: int 0 (+ i 1)))
		((= i n))
	      (out:print (digits:next)))
	    (do ((i :: int n (+ i 1)))
		((>= i L))
	      (out:print " "))
	    (set! j (+ j n))))
      (out:print "\t:")
      (out:println j))))

(pidigits (string->number (cadr (command-line))) java.lang.System:out)
