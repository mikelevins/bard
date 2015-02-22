;; The Computer Language Benchmarks Game
;;   http://shootout.alioth.debian.org/

;; Contributed by Per Bothner;
;; based on Java version by Oleg Mazurov;
;; based on original by Isaac Gouy.

(define (fannkuch (n ::int)) ::int
  (let ((perm (int[] length: n))
	(perm1 (int[] length: n))
	(count (int[] length: n))
	(max-flips-count ::int 0)
	(checksum ::int 0)
	(r ::int n))

    (do ((i :: int 0 (+ i 1))) ((= i n))
      (set! (perm1 i) i))

    (let loop-outer ((perm-count ::int 0))
      (do () ((= r 1))
	(set! (count (- r 1)) r)
	(set! r (- r 1)))
      (do ((i :: int 0 (+ i 1))) ((= i n))
	(set! (perm i) (perm1 i)))

      (let loopk ((flips-count ::int 0))
	(let ((k (perm 0)))
	  (cond ((= k 0)
		 (if (> flips-count  max-flips-count)
		     (set! max-flips-count flips-count))
		 (set! checksum (+ checksum
				   (if (odd? perm-count) (- flips-count) flips-count))))
		((let ((k2 (bitwise-arithmetic-shift-right (+ k 1) 1)))
		   (do ((i ::int 0 (+ i 1))) ((= i k2))
		     (let ((temp (perm i)) (k-i (- k i)))
		       (set! (perm i) (perm k-i))
		       (set! (perm k-i) temp)))
		   (loopk (+ flips-count 1)))))))

      (let loopr ()
	(cond ((= r n)
	       (java.lang.System:out:println checksum)
	       max-flips-count)
	      (else
	       (let ((perm0 (perm1 0)))
		 (let loopi ((i ::int 0))
		   (if (< i r)
		       (let ((j (+ i 1)))
			 (set! (perm1 i) (perm1 j))
			 (loopi j))))
		 (set! (perm1 r) perm0))
	       (set! (count r) (- (count r) 1))
	       (cond ((> (count r) 0)
		      (loop-outer (+ perm-count 1)))
		     (else
		      (set! r (+ r 1))
		      (loopr)))))))))

(define args (cdr (command-line)))
(let ((n (if (< (length args) 1) 7
	     (string->number (car args)))))
  (if (not (integer? n))
      (format #t "An integer is required~%")
      (format #t "Pfannkuchen(~S) = ~s~%" n (fannkuch n))))

