#|
The Computer Language Benchmarks Game
http://shootout.alioth.debian.org/

Contributed by Per Bothner
Based on Java version contributed by Mark C. Lewis,
modified slightly by Chad Whipkey.
|#

(define-syntax square
  (syntax-rules ()
    ((square x)
     (let ((tmp x)) (* tmp tmp)))))

(define-syntax increment
  (syntax-rules ()
    ((increment lhs rhs)
     (set! lhs (+ lhs rhs)))))

(define-syntax decrement
  (syntax-rules ()
    ((increment lhs rhs)
     (set! lhs (- lhs rhs)))))

(define-constant PI :: double 3.141592653589793)
(define-constant SOLAR_MASS :: double (* 4 PI PI))
(define-constant DAYS_PER_YEAR :: double 365.24)

(define-class Body ()
  (x :: double)
  (y :: double)
  (z :: double)
  (vx :: double)
  (vy :: double)
  (vz :: double)
  (mass :: double)
  ((offsetMomentum (px :: double) (py :: double) (pz :: double)) :: void
   (set! vx (/ (- px) SOLAR_MASS))
   (set! vy (/ (- py) SOLAR_MASS))
   (set! vz (/ (- pz) SOLAR_MASS))))

(define (nbody-system (n :: int)) :: void
  (let* ((bodies (Body[] (sun) (jupiter) (saturn) (uranus) (neptune)))
	 (px :: double 0)
	 (py :: double 0)
	 (pz :: double 0)
	 (nbodies :: int bodies:length))
    (do ((i :: int 0 (+ i 1))) ((>= i nbodies))
      (let* ((ibody (bodies i))
	     (imass ibody:mass))
	(increment px (* ibody:vx imass))
	(increment py (* ibody:vy imass))
	(increment pz (* ibody:vz imass))))
    ((bodies 0):offsetMomentum px py pz)
    (format #t "~,9f~%" (energy bodies))
    (do ((i :: int 0 (+ i 1))) ((>= i n))
      (advance bodies 0.01))
    (format #t "~,9f~%" (energy bodies))))

(define (advance (bodies :: Body[]) (dt :: double)) :: void
  (let ((nbodies bodies:length))
    (do ((i :: int 0 (+ i 1)))  ((>= i nbodies))
      (let ((ibody (bodies i)))
	(do ((j :: int (+ i 1) (+ j 1)))  ((>= j nbodies))
	  (let* ((jbody (bodies j))
		 (dx (- ibody:x jbody:x))
		 (dy (- ibody:y jbody:y))
		 (dz (- ibody:z jbody:z))
		 (dsquared (+ (square dx) (square dy) (square dz)))
		 (distance (java.lang.Math:sqrt dsquared))
		 (mag (/ dt (* dsquared distance))))
	    (decrement ibody:vx (* dx jbody:mass mag))
	    (decrement ibody:vy (* dy jbody:mass mag))
	    (decrement ibody:vz (* dz jbody:mass mag))
	    (increment jbody:vx (* dx ibody:mass mag))
	    (increment jbody:vy (* dy ibody:mass mag))
	    (increment jbody:vz (* dz ibody:mass mag))))))
    (do ((i :: int 0 (+ i 1)))  ((>= i nbodies))
      (let ((body (bodies i)))
	(increment body:x (* dt body:vx))
	(increment body:y (* dt body:vy))
	(increment body:z (* dt body:vz))))))

(define (energy (bodies :: <Body[]>)) :: double
  (let ((e :: double 0)
	(nbodies bodies:length))
    (do ((i :: int 0 (+ i 1))) ((>= i nbodies))
      (let ((ibody (bodies i)))
	(increment e (* 0.5 ibody:mass
			(+ (square ibody:vx)
			   (square ibody:vy)
			   (square ibody:vz))))
	(do ((j :: int (+ i 1) (+ j 1))) ((>= j nbodies))
	  (let* ((jbody (bodies j))
		 (dx (- ibody:x jbody:x))
		 (dy (- ibody:y jbody:y))
		 (dz (- ibody:z jbody:z))
		 (distance (java.lang.Math:sqrt (+ (square dx) (square dy) (square dz)))))
	    (decrement e (/ (* ibody:mass jbody:mass) distance))))))
    e))

(define (jupiter) :: Body
  (Body x: 4.84143144246472090e+00
	y: -1.16032004402742839e+00
	z: -1.03622044471123109e-01
	vx: (* 1.66007664274403694e-03 DAYS_PER_YEAR)
	vy: (* 7.69901118419740425e-03 DAYS_PER_YEAR)
	vz: (* -6.90460016972063023e-05 DAYS_PER_YEAR)
	mass: (* 9.54791938424326609e-04 SOLAR_MASS)))

(define (saturn) :: Body
  (Body x: 8.34336671824457987e+00
	y: 4.12479856412430479e+00
	z: -4.03523417114321381e-01
	vx: (* -2.76742510726862411e-03 DAYS_PER_YEAR)
	vy: (* 4.99852801234917238e-03 DAYS_PER_YEAR)
	vz: (* 2.30417297573763929e-05 DAYS_PER_YEAR)
	mass: (* 2.85885980666130812e-04 SOLAR_MASS)))

(define (uranus) :: Body
  (Body x: 1.28943695621391310e+01
	y: -1.51111514016986312e+01
	z: -2.23307578892655734e-01
	vx: (* 2.96460137564761618e-03 DAYS_PER_YEAR)
	vy: (* 2.37847173959480950e-03 DAYS_PER_YEAR)
	vz: (* -2.96589568540237556e-05 DAYS_PER_YEAR)
	mass: (* 4.36624404335156298e-05 SOLAR_MASS)))

(define (neptune) :: Body
  (Body x: 1.53796971148509165e+01
	y: -2.59193146099879641e+01
	z: 1.79258772950371181e-01
	vx: (* 2.68067772490389322e-03 DAYS_PER_YEAR)
	vy: (* 1.62824170038242295e-03 DAYS_PER_YEAR)
	vz: (* -9.51592254519715870e-05 DAYS_PER_YEAR)
	mass: (* 5.15138902046611451e-05 SOLAR_MASS)))

(define (sun) :: Body
  (Body mass: SOLAR_MASS))

(nbody-system (string->number (cadr (command-line))))

