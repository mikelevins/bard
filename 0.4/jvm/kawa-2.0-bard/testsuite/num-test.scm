(test-begin "num" 1881)

(test-equal 7 (+ 3 4))
(test-equal 3 (+ 3))
(test-equal 0 (+))
(test-equal +inf.0 (+ +inf.0 +inf.0))
(test-equal +nan.0 (+ +inf.0 -inf.0))
(test-equal 4 (* 4))
(test-equal 1 (*))
(test-equal +inf.0 (* 5 +inf.0))
(test-equal -inf.0 (* -5 +inf.0))
(test-equal +inf.0 (* +inf.0 +inf.0))
(test-equal -inf.0 (* +inf.0 -inf.0))
(test-equal +nan.0 (* 0 +inf.0))
(test-equal +nan.0 (* 0 +nan.0))
(test-equal 0.0 (* 1.0 0))

(test-equal 0.0  (+  0.0 -0.0))
(test-equal 0.0  (+ -0.0  0.0))
(test-equal 0.0  (+  0.0  0.0))
(test-equal -0.0 (+ -0.0 -0.0))

(test-equal -1 (- 3 4))
(test-equal -6 (- 3 4 5))
(test-equal -3 (- 3))
(test-equal +nan.0 (- +inf.0 +inf.0))

(test-equal -0.0 (-  0.0))
(test-equal  0.0 (- -0.0))
(test-equal  0.0 (-  0.0 -0.0))
(test-equal -0.0 (- -0.0  0.0))
(test-equal  0.0 (-  0.0  0.0))

(test-equal -1 (- 3 4))
(test-equal -6 (- 3 4 5))
(test-equal -3 (- 3))
(test-equal +nan.0 (- +inf.0 +inf.0))

(test-equal 3/20 (/ 3 4 5))
(test-equal 1/3 (/ 3))
(test-equal +inf.0 (/ 0.0))
(test-equal +inf.0 (/ 1.0 0))
(test-equal -inf.0 (/ -1 0.0))
(test-equal 0.0 (/ +inf.0))
;(test-error (/ 0 0))
;(test-error (/ 3 0))
(test-equal 0.0 (/ 0 3.5))
(test-equal +nan.0 (/ 0 0.0))
(test-equal +nan.0 (/ 0.0 0))
(test-equal +nan.0 (/ 0.0 0.0))

(test-equal 12 (div 123 10))
(test-equal -12 (div 123 -10))
(test-equal -13 (div -123 10))
(test-equal 13 (div -123 -10))
(test-equal 3 (mod 123 10))
(test-equal 3 (mod 123 -10))
(test-equal 7 (mod -123 10))
(test-equal 7 (mod -123 -10))
(test-equal 12 (div0 123 10))
(test-equal -12 (div0 123 -10))
(test-equal -12 (div0 -123 10))
(test-equal 12 (div0 -123 -10))
(test-equal 3 (mod0 123 10))
(test-equal 3 (mod0 123 -10))
(test-equal -3 (mod0 -123 10))
(test-equal -3 (mod0 -123 -10))
(test-equal 123 (mod 123 0))

(test-equal -1.0 (remainder -13 -4.0))

(test-equal #(-12 3) (let-values (((r q) (div-and-mod 123 -10)))
		       (vector r q)))
(test-equal #(-12 -3) (let-values (((x y) (div0-and-mod0 -123 10)))
		       (vector x y)))

(test-equal 7 (abs -7))
(test-equal +inf.0 (abs -inf.0))

(test-equal 3 (numerator (/ 6 4)))
(test-equal 2 (denominator (/ 6 4)))
(test-equal 1 (denominator 0))

(test-equal -5.0 (floor -4.3))
(test-equal -4.0 (ceiling -4.3))
(test-equal -4.0 (truncate -4.3))
(test-equal -4.0 (round -4.3))
(test-equal 3.0 (floor 3.5))
(test-equal 4.0 (ceiling 3.5))
(test-equal 3.0 (truncate 3.5))
(test-equal 4.0 (round 3.5))
(test-equal 4 (round 7/2))
(test-equal 7 (round 7))

(test-equal +inf.0 (floor +inf.0))
(test-equal -inf.0 (ceiling -inf.0))
(test-equal +nan.0 (round +nan.0))

(test-equal +inf.0 (exp +inf.0))
(test-equal 0.0 (exp -inf.0))
(test-equal +inf.0 (log +inf.0))
(test-equal -inf.0 (log 0.0))
(test-equal +inf.0+3.141592653589793i (log -inf.0))    ; approximately
(test-approximate -1.5707963267948965 (atan -inf.0) 0.00000001)
(test-approximate 1.5707963267948965 (atan +inf.0) 0.00000001)
(test-equal 0.0+3.141592653589793i (log -1.0+0.0i))
(test-equal 0.0-3.141592653589793i (log -1.0-0.0i))

(test-equal 0.0+2.23606797749979i (sqrt -5))
(test-equal +inf.0 (sqrt +inf.0))
(test-equal +inf.0i (sqrt -inf.0))

(test-approximate 1.4 (sqrt 2) 0.02)
(test-error
   #t (test-read-eval-string "0.0.0"))
(test-assert
 (eq? 3 3))

(test-equal '(2 0) (call-with-values (lambda () (exact-integer-sqrt 4)) list))
(test-equal '(2 1) (call-with-values (lambda () (exact-integer-sqrt 5)) list))

;; A problem posed by Ken Dickey (kend@data.UUCP) on comp.lang.lisp
;; to check numerical exactness of Lisp implementations.
(define (dickey-test x y)
  (+  (* 1335/4 (expt y 6))
      (* (expt x 2)
	 (- (* 11 (expt x 2) (expt y 2))
	    (expt y 6)
	    (* 121 (expt y 4))
	    2))
      (* 11/2 (expt y 8))
      (/ x (* 2 y))))
(test-eqv -54767/66192 (dickey-test 77617 33096))

(test-eqv -1/10000000000000 (/ -1 #e1e13))
(test-eqv 9223372036854775808 (- (- (expt 2 63))))
(test-eqv #i1/0 (+ 1e4294967297))
(test-eqv #i1/0 (* 1e429496729742942949672974967297 1))

(test-eqv 500000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
      (quotient (expt 10 200) (* 20 (expt 10 100))))

(test-eqv
 "neg-test" 0 (+ 17280012451545786657095548928 -17280012451545786657095548928))
(test-eqv 1250120440709706990357803482218496
	  (+ 1250137720722158536144460577767424 -17280012451545786657095548928))
(test-eqv 100000000000000
	  (quotient 10000000000000000000000000000000000 100000000000000000000))
(test-eqv 1250120440709706990357803482218496
	  (- 1250137720722158536144460577767424 17280012451545786657095548928))
(test-eqv -1250120440709706990357803482218496
	  (- 17280012451545786657095548928 1250137720722158536144460577767424))

(test-eqv #t (zero? +0.0))
(test-eqv #t (zero? -0.0))
(test-eqv #f (zero? +nan.0))
(test-eqv #t (positive? +inf.0))
(test-eqv #t (negative? -inf.0))
(test-eqv #f (positive? +nan.0))
(test-eqv #f (negative? +nan.0))
(test-eqv #f (finite? +inf.0))
(test-eqv #t (finite? 5))
(test-eqv #t (finite? 5.0))
(test-eqv #f (infinite? 5.0))
(test-eqv #t (infinite? +inf.0))

(test-group "expt"
	    (test-eqv 9223372036854775808 (expt 2 63)))

(test-begin "convert")
(test-eqv 10000000000 (inexact->exact (exact->inexact 10000000000)))
(test-eqv 1.4285714285714286e22 (exact->inexact 14285714285714285714285))
(test-eqv 0 (inexact->exact 0.0))
(test-eqv 123451/10 (rationalize (inexact->exact 12345.1) (inexact->exact 0.00001)))

(test-equal 1/3 (rationalize (exact .3) 1/10))
(test-equal #i1/3 (rationalize .3 1/10)) ; approximately
(test-equal +inf.0 (rationalize +inf.0 3))
(test-equal +nan.0 (rationalize +inf.0 +inf.0))
(test-expect-fail 1)
(test-equal 0.0 (rationalize 3 +inf.0))

(test-end "convert")

(test-begin "magnitude")
(test-eqv 4.0( magnitude 4.))
(test-eqv 4e3 (magnitude -4000.))
(test-eqv 5.0 (magnitude 3-4i))
(test-eqv 3/2 (magnitude (/ 6 -4)))
(test-end "magnitude")


(test-begin "shift")
(test-eqv #b1000 (bitwise-arithmetic-shift #b1 3))
(test-eqv #b101 (bitwise-arithmetic-shift #b1010 -1))
(test-eqv 12676506002282294014967032053760 (arithmetic-shift 10 100))
(test-end "shift")

(test-begin "bitwise")
(test-eqv #b1000 (bitwise-and #b1100 #b1010))
(test-eqv #b1110 (bitwise-ior #b1100 #b1010))
(test-eqv #b110 (bitwise-xor #b1100 #b1010))
(test-equal "-10000001" (number->string (lognot #b10000000) 2))
(test-equal "-1" (number->string (lognot #b0) 2))

(test-equal '(-1 0 1 0 2 0 1 0 3 0 1 0 2 0 1 0 4)
	    (map bitwise-first-bit-set
		 '(0 -1 -2 -3 -4 -5 -6 -7 -8 -9 -10 -11 -12 -13 -14 -15 -16)))
(test-equal '(-1 0 1 0 2 0 1 0 3 0 1 0 2 0 1 0 4)
	    (map bitwise-first-bit-set
		 '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16)))
(test-equal '(#t #f #t #t #f)
	    (map (lambda (bitno) (bitwise-bit-set? #b1101 bitno)) '(0 1 2 3 4)))
(test-eqv 1 (bitwise-copy-bit 0 0 1))
(test-eqv #b100 (bitwise-copy-bit 0 2 1))
(test-eqv #b1011 (bitwise-copy-bit #b1111 2 0))
(test-eqv #b1010 (bitwise-bit-field #b1101101010 0 4))
(test-eqv #b10110 (bitwise-bit-field #b1101101010 4 9))
(test-eqv #b1101100000 (bitwise-copy-bit-field #b1101101010 0 4 0))
(test-eqv #b1101101111 (bitwise-copy-bit-field #b1101101010 0 4 -1))
(test-eqv #b110100111110000 (bitwise-copy-bit-field #b110100100010000 5 9 -1))
(test-eqv #b10 (bitwise-rotate-bit-field #b0100 0 4 3))
(test-eqv #b10 (bitwise-rotate-bit-field #b0100 0 4 -1)) ;; Extension
(test-eqv #b110100010010000 (bitwise-rotate-bit-field #b110100100010000 5 9 -1))
(test-eqv #b110100000110000 (bitwise-rotate-bit-field #b110100100010000 5 9 1))
(test-eqv #xe5 (bitwise-reverse-bit-field #xa7 0 8))
(test-eqv #xabcdefabcdefabcdefabcdf7
	  (bitwise-reverse-bit-field #xabcdefabcdefabcdefabcdef 0 8))
(test-eqv #xe013 (bitwise-reverse-bit-field #xf3 5 16))
(test-eqv #xe013aaaaaaaaaaaaaaaa
	  (bitwise-reverse-bit-field #xf3aaaaaaaaaaaaaaaa 69 80))
(test-end)


(test-begin "logcount/bitwise-count")
(test-eqv 4 (bitwise-bit-count #b10101010))
(test-eqv 0 (bitwise-bit-count 0))
(test-eqv 1 (logcount -2))
(test-eqv -2 (bitwise-bit-count -2))
(test-eqv 3 (logcount 13))
(test-eqv 2 (logcount -13))
(test-eqv 4 (logcount 30))
(test-eqv 4 (logcount -30))
(test-end "logcount/bitwise-count")

(test-begin "gcd")
(test-eqv 3 (gcd 4294967295 3))
(test-eqv 3 (gcd 4294967298 3))
(test-eqv 2874009600 (gcd 1307674368000 2874009600))
(test-end "gcd")

(test-begin "numerical operations")
(test-eqv #t (complex? 3+4i))
(test-eqv #t (complex? 3))
(test-eqv #t (real? 3))
(test-eqv #f (real? -2.5+0.0i))
(test-eqv #t (real? -2.5+0i))
(test-eqv #t (real? -2.5))
(test-eqv #t (real? #e1e10))
(test-eqv #t (rational? 6/10))
(test-eqv #t (rational? 6/3))
(test-eqv #t (rational? 2))
(test-eqv #t (integer? 3+0i))
(test-eqv #t (integer? 3.0))
(test-eqv #t (integer? 8/4))

(test-eqv #t (number? +nan.0))
(test-eqv #t (complex? +nan.0))
(test-eqv #t (real? +nan.0))
(test-eqv #f (rational? +nan.0))
(test-eqv #t (complex? +inf.0))
(test-eqv #t (real? -inf.0))
(test-eqv #f (rational? -inf.0))
(test-eqv #f (integer? -inf.0))

(test-eqv #t (real-valued? +nan.0))
(test-eqv #t (real-valued? +nan.0+0i))
(test-eqv #t (real-valued? -inf.0))
(test-eqv #t (real-valued? 3))

(test-eqv #t (real-valued? -2.5+0.0i))
(test-eqv #t (real-valued? -2.5+0i))
(test-eqv #t (real-valued? -2.5))
(test-eqv #t (real-valued? #e1e10))

(test-eqv #f (rational-valued? +nan.0))
(test-eqv #f (rational-valued? -inf.0))
(test-eqv #t (rational-valued? 6/10))
(test-eqv #t (rational-valued? 6/10+0.0i))
(test-eqv #t (rational-valued? 6/10+0i))
(test-eqv #t (rational-valued? 6/3))

(test-eqv #t (integer-valued? 3+0i))
(test-eqv #t (integer-valued? 3+0.0i))
(test-eqv #t (integer-valued? 3.0))
(test-eqv #t (integer-valued? 3.0+0.0i))
(test-eqv #t (integer-valued? 8/4))

(test-eqv #t (exact? 5))
(test-eqv #t (inexact? +inf.0))

(test-eqv #t (integer? (java.math.BigDecimal "345")))
(test-eqv #f (integer? (java.math.BigDecimal "345.01")))
(test-eqv #t (integer? (java.lang.Long "345")))
(test-eqv #t (integer? (java.lang.Double "345")))
(test-eqv #t (exact-integer? (java.lang.Short "345")))
(test-eqv #f (exact-integer? (java.lang.Double "345")))
(test-eqv #f (exact-integer? (java.math.BigDecimal "345")))

(test-end "numerical operations")

(test-begin "logop")

;; A Boolean 1-bit version of logop.
(define (logop-bits op x y)
  (odd? (quotient op (* (if x 1 4) (if y 1 2)))))

(define (logop-compare result op x y)
  (do ((i 0 (+ i 1)))
      ((or (= i 100)
	   (not (eq? (logop-bits op (bitwise-bit-set? x i) (bitwise-bit-set? y i))
		     (bitwise-bit-set? result i))))
       i)
    #t))

(define (logop-test1 op x y)
  (logop-compare (logop op x y) op x y))

(define test-vals '(0 1 -1 2 -2 3 -3 #x7fffffff
		      #x-f0f0cccc12345 #x1234567890abcdef0012345))

(define (logop-test op)
  (do ((xl test-vals (cdr xl)))
      ((null? xl) #t)
    (do ((yl test-vals (cdr yl)))
      ((null? yl) #t)
      (test-eqv 100 (logop-test1 op (car xl) (car yl))))))

(do ((i 0 (+ i 1)))
    ((= i 16) #t)
  (logop-test i))
(test-end "logop")

(test-group
 "integer-length"
 (test-eqv 8 (bitwise-length #b10101010))
 (test-eqv 4 (bitwise-length #b1111))
 (test-eqv 0 (integer-length 0))
 (test-eqv 1 (integer-length 1))
 (test-eqv 2 (integer-length 3))
 (test-eqv 3 (integer-length 4))
 (test-eqv 3 (integer-length 7))
 (test-eqv 0 (integer-length -1))
 (test-eqv 2 (integer-length -4))
 (test-eqv 3 (integer-length -7))
 (test-eqv 3 (integer-length -8))
 (test-eqv 31 (integer-length #x7fffffff))
 (test-eqv 32 (integer-length #xffffffff))
 (test-eqv 33 (integer-length #x100000000)))

(test-eqv 1000000000000000000000000000000 (* 1000000000000000 1000000000000000))

;; From Norman Hardy <norm@netcom.com>
(define (ssin x) (let ((a2 (quotient (* x x) dx)))
   (- x (let tail ((term x)(ndx 2))
      (let ((x (quotient (* a2 term) (* ndx (+ ndx 1) dx))))
         (if (zero? x) 0 (- x (tail x (+ ndx 2)))))))))
(define dx (expt 10 100))
(define pi
  31415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170679)
(test-eqv 3 (ssin pi))

(test-eqv #f (= (expt 2. 100) (+ (expt 2 100) 1)))
(test-eqv #t (= (expt 2. 100) (exact->inexact (+ (expt 2 100) 1))))

(test-eqv 2650239300 (remainder 14853098170650239300 4000000000))

(test-equal "8000000000000000"( number->string #x8000000000000000 16))
(test-equal "80000000000000000"( number->string #x80000000000000000 16))
;; From Aubrey Jaffer <agj@alum.mit.edu>
(test-eqv #t (number? (string->number "#i0/0+1i")))
(test-eqv #t (number? (string->number "#i1+0/0i")))
(test-eqv #t (positive? 2147483648))
(test-eqv #t (negative? (string->number "#i-1/0")))

;; From Sven.Hartrumpf@fernuni-hagen.de
(define quotient-fix-1
  (lambda (a b x) (quotient (+ (quotient (* a x 10) b) 5) 10)))
(test-eqv 950 (quotient-fix-1 95 100 1000))
;; Variations on Sven's test:
(define (quotient-fix-2 (a :: <real>))
  (quotient (+ a 20) 10))
(test-eqv 97 (quotient-fix-2 950))
(define (quotient-float (a :: <real>))
  (quotient (+ a 25.0) 10))
(test-eqv 97.0 (quotient-float 950))

(define v (vector 3 -4/5 9 10 (+ 8 1/2) -100))
(java.util.Collections:sort v)
(test-equal "sort-v-1" #(-100 -4/5 3 17/2 9 10) v)
(set! v (vector 1.2 -1.2 8.9 100.0 8.9))
(java.util.Collections:sort v)
(test-equal "sort-v-2" #(-1.2 1.2 8.9 8.9 100.0) v)
(set! v (vector 1 0.5 5/2 8 2.5))
(java.util.Collections:sort v)
(test-equal  "sort-v-3" #(0.5 1 5/2 2.5 8) v)
(set! v (vector "abc" "aa" "zy" ""))
(java.util.Collections:sort v)
(test-equal "sort-v-4" #("" "aa" "abc" "zy") v)
(set! v (f32vector 1.2 -1.2 8.9 100.0 8.9))
(java.util.Collections:sort v)
(test-equal "sort-v-5" #f32(-1.2 1.2 8.9 8.9 100.0) v)
(set! v (vector #s64(3 5) #s64(3 4 5) #s64(-1) #s64(-5)
	  #s64(-1 20) #s64() #s64(-1 10)))
(java.util.Collections:sort v)
(test-equal
 "sort-v-6"
 #(#s64() #s64(-5) #s64(-1) #s64(-1 10) #s64(-1 20) #s64(3 4 5) #s64(3 5))
 v)
(set! v '("abc" "aa" "zy" ""))
(java.util.Collections:sort v)
(test-assert "sort-v-7" (equal? '("" "aa" "abc" "zy") v))
(set! v (vector '(b 3) '(a 1) '(b 2) '(a 2) '(b -1) '(a)))
(java.util.Collections:sort v)
(test-equal "sort-v-8" #((a) (a 1) (a 2) (b -1) (b 2) (b 3)) v)

;; Savannah bug #11427  Dean Ferreyra <dferreyra@igc.org>
;; <java.lang.Integer> in the interpreter gives ClassCastException.
(define seven (make <java.lang.Integer> 7))(- (as <int> seven) 3)
(test-assert (= seven 7))

;; Bug reported by Alex Mitchell
(define denom 10.0)
(test-equal 0.0 (let ((numer2 0)) (/ numer2 denom)))

;; Bug reported by Alex Mitchell
(test-equal '(20.0) (let ((b (* denom 2.0))) (list b)))

(test-equal "java.lang.Float" (invoke (invoke 12.5s2 'getClass) 'getName))
(test-equal "java.lang.Float" (invoke (invoke 12.5F2 'getClass) 'getName))
(test-equal "java.lang.Double" (invoke (invoke 12.5d2 'getClass) 'getName))
(test-equal "java.math.BigDecimal" (invoke (invoke 12.5l2 'getClass) 'getName))
(test-equal "gnu.math.DFloNum" (invoke (invoke 12.5e2 'getClass) 'getName))
(test-equal "gnu.math.DFloNum" (invoke (invoke 12.5 'getClass) 'getName))

(test-assert (= 0.0s0 0.0s0))
(test-assert (eqv? 0.0s0 0.0s0))
(test-assert (equal? 0.0s0 0.0s0))
(test-assert (= 0.0s0 0.0d0))
(test-eqv #f (eqv? 0.0s0 0.0d0))
(test-eqv #f (equal? 0.0s0 0.0d0))
(test-assert (= java.lang.Double:POSITIVE_INFINITY 
                java.lang.Float:POSITIVE_INFINITY))
(test-eqv #f (equal? java.lang.Double:POSITIVE_INFINITY 
                     java.lang.Float:POSITIVE_INFINITY))

(test-assert (> 1/0 -1/0))
(test-assert (< -1/0 1/0))
(test-assert (> 4/5 -1/0))
(test-assert (< 4/5 1/0))
(test-assert (< 0 1/0))
(test-assert (> 4.5 -1/0))

(define (circle-area radius) (* java.lang.Math:PI (expt radius 2)))
(test-approximate 28.27 (circle-area 3) 0.1)

(test-assert (not (gnu.math.Complex:equals 3+4i 3+5i)))

(test-end)
