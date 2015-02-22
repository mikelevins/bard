(require <kawa.lib.prim_syntax>)
(require <kawa.lib.std_syntax>)
(require <kawa.lib.syntax>)
(require <kawa.lib.misc>)
(define-alias Double java.lang.Double)
(define-alias IntNum gnu.math.IntNum)
(define-alias Numeric gnu.math.Numeric)
(define-alias RatNum gnu.math.RatNum)
(define-alias RealNum gnu.math.RealNum)
(define-alias LangObjType gnu.kawa.lispexpr.LangObjType)
(define-alias quaternion gnu.math.Quaternion)

(define-private (java.lang.real? x) ::boolean
  (and (java.lang.Number? x)
       (or (java.lang.Long? x)
           (java.lang.Integer? x)
           (java.lang.Short? x)
           (java.lang.Byte? x)
           (java.lang.Double? x)
           (java.lang.Float? x)
           (java.math.BigInteger? x)
           (java.math.BigDecimal? x))))
(define (number? x) ::boolean (java.lang.Number? x))
(define (quantity? x) ::boolean
  (or (instance? x <quantity>)
      (java.lang.real? x)))
(define (quaternion? x) ::boolean
  (or (instance? x quaternion)
      (java.lang.real? x)))
(define (complex? x) ::boolean
  (or (instance? x <complex>)
      (java.lang.real? x)))
(define (real? x) ::boolean
  (or (instance? x <real>)
      (java.lang.real? x)))
(define (rational? x)  ::boolean
  (or (instance? x <rational>)
      (and (java.lang.Number? x)
	   (or (java.lang.Long? x)
	       (java.lang.Integer? x)
	       (java.lang.Short? x)
	       (java.lang.Byte? x)
	       (java.math.BigInteger? x)
	       (java.math.BigDecimal? x)))))

(define (integer? x) :: <boolean>
  (or (instance? x <gnu.math.IntNum>)
      (and (instance? x <java.lang.Number>)
           (cond ((or (java.lang.Long? x)
                      (java.lang.Integer? x)
                      (java.lang.Short? x)
                      (java.lang.Byte? x)
                      (java.math.BigInteger? x))
                  #t)
                 ((or (gnu.math.DFloNum? x)
                      (java.lang.Float? x)
                      (java.lang.Double? x))
                  (= (java.lang.Math:IEEEremainder
                      (java.lang.Number:doubleValue x)
                      1.0)
                     0.0))
                 ((java.math.BigDecimal? x)
                  (try-catch
                   (begin
                     ((->java.math.BigDecimal x):toBigIntegerExact)
                     #t)
                   (ex java.lang.ArithmeticException #f)))
                 (else
                  #f)))))

(define (exact-integer? x) :: <boolean>
  (or (instance? x <gnu.math.IntNum>)
      (and (instance? x <java.lang.Number>)
	   (or (instance? x <java.lang.Long>)
	       (instance? x <java.lang.Integer>)
	       (instance? x <java.lang.Short>)
	       (instance? x <java.lang.Byte>)
	       (instance? x <java.math.BigInteger>)))))

(define (real-valued? x) ::boolean
  (and (quaternion? x)
       (zero? (imag-part x))
       (zero? (jmag-part x))
       (zero? (kmag-part x))
       (real? (real-part x))))
(define (rational-valued? x) ::boolean
  (and (quaternion? x)
       (zero? (imag-part x))
       (zero? (jmag-part x))
       (zero? (kmag-part x))
       (rational? (real-part x))))
(define (integer-valued? x) ::boolean
  (and (quaternion? x)
       (zero? (imag-part x))
       (zero? (jmag-part x))
       (zero? (kmag-part x))
       (integer? (real-part x))))

(define (exact? x) :: boolean 
  (and (java.lang.Number? x)
       (gnu.kawa.functions.Arithmetic:isExact (as java.lang.Number x))))

(define (inexact? x) :: boolean
  (and (java.lang.Number? x)
       (not (gnu.kawa.functions.Arithmetic:isExact (as java.lang.Number x)))))

(define (zero? (x :: java.lang.Number)) :: boolean
  (cond ((gnu.math.Numeric? x)
	 ((as gnu.math.Numeric x):isZero))
	((java.math.BigInteger? x)
	 (= 0 (as java.math.BigInteger x):signum))
	((java.math.BigDecimal? x)
	 (= 0 (as java.math.BigDecimal x):signum))
	(else
	 (= 0.0 (x:doubleValue)))))

(define (positive? (x :: <real>)) :: <boolean>
  (> (invoke x 'sign) 0))

(define (negative? (x :: real)) :: <boolean> 
  (invoke x 'isNegative))

(define (finite? (z ::java.lang.Number)) ::boolean
  (if (gnu.math.Quaternion? z)
      (> ((->gnu.math.Quaternion z):classifyFinite) 0)
      (and (java.lang.real? z)
           (let ((d (z:doubleValue)))
             (and (not (java.lang.Double:isInfinite d))
                  (not (java.lang.Double:isNaN d)))))))

(define (infinite? (z ::java.lang.Number)) ::boolean
  (if (gnu.math.Quaternion? z)
      (let ((zc ::gnu.math.Quaternion z))
        (or (= ((zc:re):classifyFinite) 0)
            (= ((zc:im):classifyFinite) 0)
            (= ((zc:jm):classifyFinite) 0)
            (= ((zc:km):classifyFinite) 0)))
      (and (java.lang.real? z)
           (let ((d (z:doubleValue)))
             (java.lang.Double:isInfinite d)))))

(define (nan? (z ::java.lang.Number)) ::boolean
  (if (gnu.math.Quaternion? z)
      (< ((->gnu.math.Quaternion z):classifyFinite) 0)
      (and (java.lang.real? z)
           (let ((d (z:doubleValue)))
             (java.lang.Double:isNaN d)))))

(define (max #!rest (args :: <Object[]>))
  (let ((n :: <int> args:length)
	(result :: <real> (args 0)))
    (do ((i :: <int> 1 (+ i 1)))
	 ((>= i n) result)
      (set! result
	    (*:max result (args i))))))

(define (min #!rest (args :: <Object[]>))
  (let ((n :: <int> args:length)
	(result :: <real> (args 0)))
    (do ((i :: <int> 0 (+ i 1)))
	 ((>= i n) result)
      (set! result
	    (*:min result (args i))))))

(define (abs (x :: java.lang.Number)) :: java.lang.Number
  (cond ((gnu.math.Numeric? x)
	 ((as gnu.math.Numeric x):abs))
	((>= x 0)
	 x)
	(else
	 (- x))))

(define (floor/ (x :: real) (y :: real))
  (let* ((q (floor-quotient x y))
	 (r (- x (* q y))))
    (values q r)))

(define (truncate/ (x :: real) (y :: real))
  (let* ((q (truncate-quotient x y))
	 (r (- x (* q y))))
    (values q r)))

(define (div-and-mod (x :: real) (y :: real))
  (let* ((q (div x y))
	 (r (- x (* q y))))
    (values q r)))

(define (div0-and-mod0 (x :: real) (y :: real))
  (let* ((q (div0 x y))
	 (r (- x (* q y))))
    (values q r)))

(define (gcd #!rest (args ::real[])) ::real
  (let ((n args:length)
        (result-inexact ::boolean #f)
        (result ::integer 0))
    (do ((i ::int 0 (+ i 1)))
        ((>= i n)
         (if result-inexact (inexact result) result))
      (let* ((val ::real (args i))
             (cur-inexact (inexact? val))
             (cur ::integer
                  (if cur-inexact
                      (begin (set! result-inexact #t)
                             (exact val))
                      val)))
        (set! result (if (= i 0) cur
                         (gnu.math.IntNum:gcd result cur)))))))

(define (lcm #!rest (args ::real[])) ::real
  (let ((n args:length)
        (result-inexact ::boolean #f)
        (result ::integer 1))
    (do ((i ::int 0 (+ i 1)))
        ((>= i n)
         (if result-inexact (inexact result) result))
      (let* ((val ::real (args i))
             (cur-inexact (inexact? val))
             (cur ::integer
                  (if cur-inexact
                      (begin (set! result-inexact #t)
                             (exact val))
                      val)))
        (set! result (if (= i 0) cur
                         (gnu.math.IntNum:lcm result cur)))))))

(define (numerator (x ::real)) ::real
  (if (RatNum? x)
      ((->RatNum x):numerator)
      (inexact ((LangObjType:coerceRatNum (exact x)):numerator))))

(define (denominator (x ::real)) ::real
  (if (RatNum? x)
      ((->RatNum x):denominator)
      (inexact ((LangObjType:coerceRatNum (exact x)):denominator))))

(define (floor (x :: real)) :: real
  (x:toInt gnu.math.Numeric:FLOOR))

(define (ceiling (x :: real)) :: real
  (x:toInt gnu.math.Numeric:CEILING))

(define (truncate (x :: real)) :: real
  (x:toInt gnu.math.Numeric:TRUNCATE))

(define (round (x :: real)) :: real
  (x:toInt gnu.math.Numeric:ROUND))

(define (rationalize (x :: <real>) (y :: <real>)) :: <real>
  (gnu.math.RatNum:rationalize
   (as <real> (invoke x 'sub y))
   (as <real> (invoke x 'add y))))

(define (exp (x ::java.lang.Number)) ::java.lang.Number
  (cond ((java.lang.real? x) (java.lang.Math:exp x))
        ((gnu.math.Quaternion? x) ((->gnu.math.Quaternion x):exp))
        (else (primitive-throw (java.lang.IllegalArgumentException)))))

(define-procedure log
  (lambda ((x ::java.lang.Number) (base ::java.lang.Number))
    ::java.lang.Number
    (cond ((and (gnu.math.RealNum? x) (gnu.math.RealNum? base))
           (/ (log x) (log base)))
          ((and (or (java.lang.real? x) (gnu.math.RealNum? x))
                (or (java.lang.real? base) (gnu.math.RealNum? base)))
           (/ (java.lang.Math:log x) (java.lang.Math:log base)))
          (else (/ (log x) (log base)))))
  (lambda (x ::java.lang.Number)
    ::java.lang.Number
    (cond ((java.lang.real? x) (java.lang.Math:log x))
          ((gnu.math.Quaternion? x)
           ((->gnu.math.Quaternion x):log)))))

(define-procedure sin
  (lambda (q ::quaternion) ::quaternion
          (invoke q 'sin))
  (lambda (x ::double) ::double
          (invoke-static <java.lang.Math> 'sin x)))

(define-procedure cos
  (lambda (q ::quaternion) ::quaternion
          (invoke q 'cos))
  (lambda (x ::double) ::double
          (invoke-static <java.lang.Math> 'cos x)))

(define-procedure tan
  (lambda (q ::quaternion) ::quaternion
          (invoke q 'tan))
  (lambda (x ::double) ::double
          (invoke-static <java.lang.Math> 'tan x)))

(define (asin (x ::java.lang.Number)) ::java.lang.Number
  (cond ((java.lang.real? x)
         (java.lang.Math:asin x))
        ((and (real-valued? x) (<= -1 x 1))
         (gnu.math.DFloNum (java.lang.Math:asin (x:doubleValue))))
        (else
         (let* ((q ::gnu.math.Quaternion
                   (gnu.kawa.functions.Arithmetic:asNumeric x))
                (u (unit-vector q))
                (v (if (= 0 u) +i u)))
           (- (* v (log (+ (* v q) (sqrt (- 1 (* q q)))))))))))

(define (acos (x ::java.lang.Number)) ::java.lang.Number
  (cond ((java.lang.real? x)
         (java.lang.Math:acos x))
        ((and (real-valued? x) (<= -1 x 1))
         (gnu.math.DFloNum (java.lang.Math:acos (x:doubleValue))))
        (else
         (let* ((q ::gnu.math.Quaternion
                   (gnu.kawa.functions.Arithmetic:asNumeric x))
                (u (unit-vector q))
                (v (if (= 0 u) +i u)))
           (- (* v (log (+ q (sqrt (- (* q q) 1))))))))))

(define-procedure atan
  (lambda ((y ::java.lang.Number) (x ::java.lang.Number))
    ::java.lang.Number
    (cond ((and (gnu.math.RealNum? y) (gnu.math.RealNum? x))
           (gnu.math.DFloNum (java.lang.Math:atan2 (y:doubleValue)
                                                   (x:doubleValue))))
          ((and (or (java.lang.real? y) (gnu.math.RealNum? y))
                (or (java.lang.real? x) (gnu.math.RealNum? x)))
           (java.lang.Math:atan2 (y:doubleValue) (x:doubleValue)))
          (else
           (primitive-throw (java.lang.IllegalArgumentException)))))
  (lambda (x ::java.lang.Number)
    ::java.lang.Number
    (cond ((gnu.math.RealNum? x)
           (gnu.math.DFloNum (java.lang.Math:atan (x:doubleValue))))
          ((java.lang.real? x)
           (java.lang.Math:atan (x:doubleValue)))
          (else
           (let* ((q ::gnu.math.Quaternion x)
                  (u (unit-vector q))
                  (v (if (= 0 u) +i u)))
             (* 1/2 v (log (* (+ v q) (/ (- v q))))))))))

(define (sqrt x::java.lang.Number) ::java.lang.Number
  (cond ((java.lang.real? x)
         (java.lang.Math:sqrt x))
        ((gnu.math.Quantity? x)
         (let ((num ::quantity x))
           (gnu.math.Quantity:make ((num:number):sqrt)
                                   ((num:unit):sqrt))))))

(define (square x::quantity) ::quantity
  (* x x))

(define-procedure make-rectangular
  (lambda (x::real y::real) ::complex
          (invoke-static <complex> 'make x y))
  (lambda (w::real x::real y::real z::real) ::quaternion
          (quaternion:make w x y z)))

(define-procedure make-polar
  (lambda (x::double y::double) ::complex
          (invoke-static <complex> 'polar x y))
  (lambda (r::double t::double u::double v::double) ::quaternion
          (quaternion:polar r t u v)))

(define (real-part (x ::java.lang.Number)) ::java.lang.Number
  (if (gnu.math.Quaternion? x)
      ((->gnu.math.Quaternion x):re)
      x))

(define (imag-part (x ::java.lang.Number)) ::java.lang.Number
  (if (gnu.math.Quaternion? x)
      ((->gnu.math.Quaternion x):im)
      (gnu.math.IntNum:zero)))

(define (jmag-part x::java.lang.Number) ::java.lang.Number
  (if (gnu.math.Quaternion? x)
      ((->gnu.math.Quaternion x):jm)
      (gnu.math.IntNum:zero)))

(define (kmag-part x::java.lang.Number) ::java.lang.Number
  (if (gnu.math.Quaternion? x)
      ((->gnu.math.Quaternion x):km)
      (gnu.math.IntNum:zero)))

(define (unit-vector x::java.lang.Number) ::quaternion
  (if (gnu.math.Quaternion? x)
      ((->gnu.math.Quaternion x):unitVector)
      0))

(define (magnitude (x :: java.lang.Number)) :: java.lang.Number
  (abs x))

(define (angle (x ::java.lang.Number)):: <real>
  (if (gnu.math.Quaternion? x)
      ((->gnu.math.Quaternion x):angle)
      (if (< (x:doubleValue) 0) java.lang.Math:PI 0)))

(define (inexact (num :: java.lang.Number)) :: java.lang.Number
  (gnu.kawa.functions.Arithmetic:toInexact num))

(define (exact (num :: java.lang.Number)) :: java.lang.Number
  (gnu.kawa.functions.Arithmetic:toExact num))

(define (exact->inexact (num :: java.lang.Number)) :: java.lang.Number
  (gnu.kawa.functions.Arithmetic:toInexact num))

(define (inexact->exact (num :: java.lang.Number)) :: java.lang.Number
  (gnu.kawa.functions.Arithmetic:toExact num))

(define (logop (op :: <int>) (i :: <integer>) (j :: <integer>)) :: <integer>
  (invoke-static <gnu.math.BitOps> 'bitOp op i j))

(define (bitwise-bit-set? (i :: <integer>) (bitno :: <int>)) :: <boolean>
  (gnu.math.BitOps:bitValue i bitno))

(define (bitwise-copy-bit (i :: integer) (bitno :: int) (new-value :: int))
  :: integer
  (gnu.math.BitOps:setBitValue i bitno new-value))

(define (bitwise-copy-bit-field (to :: integer) (start :: int) (end :: int) (from :: integer)) ::  integer
  (let* ((mask1 (gnu.math.IntNum:shift -1 start))
	 (mask2 (gnu.math.BitOps:not (gnu.math.IntNum:shift -1 end)))
	 (mask (gnu.math.BitOps:and mask1 mask2)))
    (bitwise-if mask
		(gnu.math.IntNum:shift from start)
		to)))

(define (bitwise-bit-field (i :: <integer>) (start :: <int>) (end :: <int>))
  :: <integer>
  (invoke-static <gnu.math.BitOps> 'extract i start end))

(define (bitwise-if (e1 :: integer) (e2 :: integer) (e3  integer)) :: integer
  (gnu.math.BitOps:ior (gnu.math.BitOps:and e1 e2)
		       (gnu.math.BitOps:and (gnu.math.BitOps:not e1) e3)))

(define (logtest (i :: <integer>) (j :: <integer>))
  (invoke-static <gnu.math.BitOps> 'test i j))

(define (logcount (i :: <integer>)) :: <int>
  (gnu.math.BitOps:bitCount
   (if (>= i 0) i (gnu.math.BitOps:not i))))

(define (bitwise-bit-count (i :: <integer>)) :: <int>
  (if (>= i 0)
      (gnu.math.BitOps:bitCount i)
      (- -1 (gnu.math.BitOps:bitCount (gnu.math.BitOps:not i)))))  

(define (bitwise-length (i :: <integer>)) :: <int>
  (invoke i 'intLength))

(define (bitwise-first-bit-set (i :: <integer>)) :: <int>
  (gnu.math.BitOps:lowestBitSet i))

(define (bitwise-rotate-bit-field (n :: integer) (start :: int) (end :: int) (count :: int)) :: integer
  (let ((width (- end start)))
    (if (> width 0)
	(let* (;; Optimization of modulo.
	       (r (remainder count width))
	       (count (if (< r 0) (+ r width) r))
	       (field0 (bitwise-bit-field n start end))
	       (field1 (gnu.math.IntNum:shift field0 count))
	       (field2 (gnu.math.IntNum:shift field0 (- count width)))
	       (field (gnu.math.BitOps:ior field1 field2)))
	  (bitwise-copy-bit-field n start end field))
	n)))

(define (bitwise-reverse-bit-field (n :: integer) (start :: int) (end :: int)) :: integer
  (gnu.math.BitOps:reverseBits n start end))

(define (number->string (arg :: <java.lang.Number>)
			#!optional (radix :: <int> 10)) ::string
  (if (or (< radix 2) (> radix 36))
      (primitive-throw (java.lang.IllegalArgumentException
                        (format #f "invalid radix ~d" radix))))
  (make <gnu.lists.FString>
    (gnu.kawa.functions.Arithmetic:toString arg radix)))

(define (string->number (str :: <string>) #!optional (radix ::int 10))
  ::object
  (if (or (< radix 2) (> radix 36))
      (primitive-throw (java.lang.IllegalArgumentException
                        (format #f "invalid radix ~d" radix))))
  (let ((result (gnu.kawa.lispexpr.LispReader:parseNumber str (- radix))))
    (if (instance? result <gnu.math.Numeric>) result #f)))

(define (quantity->number (q :: <quantity>)) ::quaternion
  (let ((u (q:unit))
	(factor (q:doubleValue)))
    (if (= factor 1.0)
	(q:number)
	(quaternion:make (q:reValue) (q:imValue) (q:jmValue) (q:kmValue)))))

(define (quantity->unit (q :: <quantity>)) :: <gnu.math.Unit>
  (q:unit))

(define (make-quantity val unit) :: <quantity>
  (let ((u :: <gnu.math.Unit>
	   (if (instance? unit <gnu.math.Unit>) unit
	       (<gnu.math.Unit>:lookup unit))))
    (if (eq? u #!null)
	(primitive-throw (<java.lang.IllegalArgumentException>
			 ((format "unknown unit: ~s" unit):toString))))
    (<quantity>:make val u)))

(define (duration duration) :: <gnu.math.Duration>
  (gnu.math.Duration:parseDuration duration))

(define (exact-integer-sqrt (i ::integer))
  (if (< i 0)
      (primitive-throw (java.lang.IllegalArgumentException
                        (format #f "negative argument: ~A" i))))
  (let* ((dval ::double (i:doubleValue))
         (init ::integer ;; initial approximation
               (if (Double:isInfinite dval) 
                   (IntNum:shift 1 (i:intLength))
                   (RealNum:toExactInt (java.lang.Math:sqrt dval)
                                       Numeric:TRUNCATE))))
    ;; "Babylonian method"
    (let loop ((q ::integer init)) ;; q is current approximation
      (let ((rem (- i (* q q))))
        ;; Continue if rem<0 or (q+1)^2<=i
        (if (or (< rem 0)
                ;; Because rem=i-q^2, we can simplify (q+1)^2<=i
                ;; to q^2+2*q+1<=q^2+rem or 2*q+1<=rem
                (<= (IntNum:shift q 1) (- rem 1)))
            ;; Next q is average of old q and i/q
            (loop (IntNum:shift (+ q (IntNum:quotient i q)) -1))
            (values q rem))))))
