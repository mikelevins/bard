;; fannkuch benchmark for The Computer Language Shootout
;; Written by Dima Dorfman, 2004
;; Slightly improved by Sven Hartrumpf, 2005-2006
;;
;; Ever-so-slightly tweaked for MzScheme by Brent Fulgham
;; Converted to Kawa by Per Bothner

(define (vector-reverse-slice! (v :: int[]) (i :: int) (j :: int)) :: void
  (let loop ((j :: int (- j 1))) ; exclude position j
    (if (< i j)
	(let ((t (v i)))
	  (set! (v i) (v j))
	  (set! (v j) t)
	  (set! i (+ i 1))
	  (loop (- j 1))))))

(define (count-flips pi::int[] pt::int[]) :: int
  (do ((i :: int 0 (+ i 1)))
      ((= i pi:length))
    (set! (pt i) (pi i)))
  (do ((i :: int 0 (+ i 1)))
        ((= (pt 0) 0) i)
      (vector-reverse-slice! pt 0 (+ (pt 0) 1))))

(define (fannkuch (n :: int)) :: int
  (let ((pi (int[] length: n))
	(pt (int[] length: n))
	(r :: int n)
	(count (int[] length: n)))
    (do ((i :: int 0 (+ i 1))) ((= i n))
      (set! (pi i) i))
    (let loop ((flips :: int 0)
	       (perms :: int 0))
      (cond ((< perms 30)
	     (do ((i :: int 0 (+ i 1)))
		 ((>= i n))
	       (format #t "~d" (+ (pi i) 1)))
	     (newline)))
      (do ()
          ((= r 1))
	(set! (count (- r 1)) r)
	(set! r (- r 1)))
      (let* ((flips1 (count-flips pi pt))
	     (flips2 (if (> flips1 flips) flips1 flips)))
	(let ((result :: int
	       (let loop2 ()
		 (if (= r n)
		     flips2
		     (let ((perm0 (pi 0)))
		       (do ((i :: int 0))
			   ((>= i r))
			 (let ((j (+ i 1)))
			   (set! (pi i) (pi j))
			   (set! i j)))
		       (set! (pi r) perm0)
		       (set! (count r) (- (count r) 1))
		       (cond ((<= (count r) 0)
			      (set! r (+ r 1))
			      (loop2))
			     (else
			      -1)))))))
	  (if (>= result 0) result
	      (loop flips2 (+ perms 1)))
	  )))))

(define args (cdr (command-line)))
(if (< (length args) 1)
    (begin (display "An argument is required") (newline) 2)
    (let ((n (string->number (car args))))
      (if (not (integer? n))
	  (format #t "An integer is required~%")
	  (format #t "Pfannkuchen(~S) = ~s~%" n (fannkuch n)))))
