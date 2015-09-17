#|
The Computer Language Benchmarks Game
http://shootout.alioth.debian.org/

Contributed by Per Bothner
Loosely based on Java version contributed by Anthony Donnefort
and slightly modified to read 82 bytes at a time by Razii.
|#

(define-constant output-buffer-size :: int 8192)

(define-constant cmp :: byte[]
  (let* ((c (byte[] length: 128))
	 (from "TAGCVHRMYKBDU")
	 (to "ATCGBDYKRMVHA")
	 (i :: int (string-length from)))
    (do ((i :: int 0 (+ i 1))) ((>= i 128))
      (set! (c i) i))
    (let loop ()
      (set! i (- i 1))
      (let ((f (char->integer (string-ref from i)))
	    (t (char->integer (string-ref to i))))
	(set! (c f) t)
	(set! (c (+ f 32)) t))
      (if (> i 0)
	  (loop)))
    c))

(define (revcomp (in :: java.io.InputStream)) :: void
  (let* ((line (byte[] length: 82))
	 (i :: int 0) ;; index in line
	 (read :: int 0) ;; used size of line
	 (last :: int 0) ;; line[j], j<last, has been copied to buf
	 (bsize :: int output-buffer-size)
	 (bcount :: int 0) ;; used amount in buf
	 (buf :: byte[] (byte[] length: bsize))
	 (GT :: int (char->integer #\>))
	 (NL :: int (char->integer #\NewLine)))
    (let loop ()
      (let ((copy-needed :: boolean
			   (or (= i read) (= (line i) GT))))
	(if copy-needed
	    (let* ((added (- i last))
		   (needed (+ bcount added)))
	      (if (> needed bsize)
		  (let* ((newsize (+ needed bsize))
			 (newbuf (byte[] length: newsize)))
		    (java.lang.System:arraycopy buf 0 newbuf 0 bcount)
		    (set! bsize newsize)
		    (set! buf newbuf)))
	      (java.lang.System:arraycopy line last buf bcount added)
	      (set! bcount (+ bcount added))
	      (set! last i)))
	(cond ((= i read)
	       (set! read (in:read line))
	       (set! i -1)
	       (set! copy-needed #f)
	       (set! last 0)))
	(cond ((and (or (< read 0) copy-needed)
		    (> bcount 0))
	       ;; do the reverse ...
	       (let ((j :: int 0)  (k :: int (- bcount 1)))
		 (do ()
		     ((let ((b :: int (buf j)))
			(set! j (+ j 1))
			(= b NL))))
		 (do ()  ((> j k))
		   (if (= (buf j) NL)
		       (set! j (+ j 1)))
		   (if (= (buf k) NL)
		       (set! k (- k 1)))
		   (if (<= j k)
		       (let ((tmp (buf j)))
			 (set! (buf j) (cmp (buf k)))
			 (set! (buf k) (cmp tmp))
			 (set! j (+ j 1))
			 (set! k (- k 1))))))
	       (java.lang.System:out:write buf 0 bcount)
	       (set! bcount 0)))
	(set! i (+ i 1))
	(if (>= read 0)
	    (loop))))))

(revcomp java.lang.System:in)
