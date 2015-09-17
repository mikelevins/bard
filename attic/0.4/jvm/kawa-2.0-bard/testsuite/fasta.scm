;;; The Computer Language Benchmarks Game
;;; http://shootout.alioth.debian.org/
;;;
;;; Kawa version by Per Bothner inspired by a combination of
;;; (1) PLT version contributed by Matthew Flatt
;;; derived from the Chicken variant by Anthony Borla
;;; (2) Java version "modified by Mehmet D. AKIN"

(define ALU :: byte[]
  ("GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGG\
    GAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAGA\
    CCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAAT\
    ACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCA\
    GCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGG\
    AGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCC\
    AGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA":getBytes))

(define IUB :: vector
  #((#\a . 0.27) (#\c . 0.12) (#\g . 0.12) (#\t . 0.27) (#\B . 0.02)
    (#\D . 0.02) (#\H . 0.02) (#\K . 0.02) (#\M . 0.02) (#\N . 0.02)
    (#\R . 0.02) (#\S . 0.02) (#\V . 0.02) (#\W . 0.02) (#\Y . 0.02)))

(define HomoSapiens :: vector
  #((#\a . 0.3029549426680) (#\c . 0.1979883004921)
    (#\g . 0.1975473066391) (#\t . 0.3015094502008)))

(define-constant buffer-size :: int 2048)
(define bbuffer :: byte[] (byte[] length: buffer-size))

(define-constant line-size :: int 60)

(define (make-cumulative-table (frequency-table :: vector)) :: double[]
  (let* ((cp :: double 0.0)
	 (n :: int (vector-length frequency-table))
	 (cumulative (double[] length: n)))
    (do ((i :: int 0 (+ i 1)))
	((>= i n)
	 cumulative)
      (let ((x :: pair (frequency-table i)))
	(set! cp (+ cp (cdr x)))
	(set! (cumulative i) cp)))))

(define-private last :: int 42)

(define (random-next (max :: double)) :: double
  (let* ((im :: int 139968))
    (set! last (remainder (+ 29573 (* last 3877)) 139968))
    (/ (* max last) im)))

(define (select-random (frequency-table :: vector) (cumulative-table :: double[])) :: int
  (let ((rvalue (random-next 1.0)))
    (do ((i :: int 0 (+ i 1)))
	((<= rvalue (cumulative-table i))
	 (char->integer (car (frequency-table i)))))))

;; -------------

(define-syntax generate-fasta
  (syntax-rules ()
    ((generate-fasta id desc n_ line-length out action)
     (let ((n :: int n_)
	   (index :: int 0)
	   (NL (char->integer #\newline)))
       (out:write (((string-append ">" id " " desc "\n"):toString):getBytes))
       (do ()
	   ((<= n 0))
	 (let ((m :: int (if (< n line-length) n line-length))
	       (avail :: int (- buffer-size index)))
	   (cond ((< avail m)
		  (out:write bbuffer 0 index)
		  (set! index 0)))
	   (do ((i :: int 0 (+ i 1)))
	       ((>= i m))
	     (set! (bbuffer index) action)
	     (set! index (+ index 1)))
	   (set! (bbuffer index) NL)
	   (set! index (+ index 1))
	   (set! n (- n line-length))))
	 (if (> index 0)
	     (out:write bbuffer 0 index))))))

(define (repeat-fasta (id :: string) (desc :: string) (n :: int)
		      (ALU :: byte[])
		      (out :: java.io.OutputStream)) :: void
  (let ((k :: int 0)
	(kn :: int ALU:length))
    (generate-fasta id desc n line-size out
		    (begin
		      (if (= k kn) (set! k 0))
		      (let ((bval (ALU k)))
			(set! k (+ k 1))
			bval)))))

(define (random-fasta (id :: string) (desc :: string) (n :: int)
		      (frequency-table :: vector)
		      (out :: java.io.OutputStream)) :: void
  (let ((cumulative :: double[] (make-cumulative-table frequency-table)))
    (generate-fasta id desc n line-size out
		    (select-random frequency-table cumulative))))

(let* ((args (cdr (command-line)))
       (n :: int (string->number (args 0)))
       (out :: java.io.OutputStream java.lang.System:out))

  (repeat-fasta "ONE" "Homo sapiens alu" (* n 2) ALU out)
  (random-fasta "TWO" "IUB ambiguity codes" (* n 3) IUB out)
  (random-fasta "THREE" "Homo sapiens frequency" (* n 5) HomoSapiens out))
