;; The Computer Language Benchmarks Game
;; http://shootout.alioth.debian.org/
;; Contributed by Per Bothner
;; Based on Java version #1 contributed by Anthony Donnefort
;; slightly modified to read 82 bytes at a time by Razii

(define-constant NL :: int 10) ; #\newline as byte
(define-constant GT ::int 62)  ; #\> as byte
(define cmp :: byte[]
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

(define-simple-class ReversibleByteArray (java.io.ByteArrayOutputStream)
  class-name: ".RevByteArray"
  ((reverse-and-print) ::void ;; throws Exception FIXME
   (if (> count 0)
       (let ((i ::int 0)
	     (j ::int (- count 1))
	     (b ::byte[] buf))
	 (let loop ()
	     (let ((old (b i)))
	       (set! i (+ i 1))
	       (if (not (= old NL))
		   (loop))))
	 (let loop ()
	   (cond ((<= i j)
		  (if (= (b i) NL)
		      (set! i (+ i 1)))
		  (if (= (b j) NL)
		      (set! j (- j 1)))
		  (if (<= i j)
		      (let ((tmp (b i)))
			(set! (b i) (cmp (b j)))
			(set! i (+ i 1))
			(set! (b j) (cmp tmp))
			(set! j (- j 1))))
		  (loop))))
	 (java.lang.System:out:write b 0 count)))))

(let ((line (byte[] length: 82))
      (buf (ReversibleByteArray)))
  (let loop ()
    (let ((read (java.lang.System:in:read line)))
      (if (>= read 0)
	  (let ((last ::int 0))
	    (do ((i ::int 0 (+ i 1))) ((>= i read) #!void)
	      (cond ((= (line i) GT)
		     (buf:write line last (- i last))
		     (buf:reverse-and-print)
		     (buf:reset)
		     (set! last i))))
	    (buf:write line last (- read last))
	    (loop)))))
  (buf:reverse-and-print))

