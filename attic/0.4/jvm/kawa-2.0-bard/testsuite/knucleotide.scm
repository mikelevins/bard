;;   The Computer Language Shootout
;;   http://shootout.alioth.debian.org/

;; Based on mzscheme version,
;; with some bits based on Java version contributed by James McIlree.

(import (srfi :69 basic-hash-tables))
(import (srfi :95 sorting-and-merging))

(define (all-counts (len :: int) (dna :: java.lang.String))
  (let ((table (make-hash-table)))
    (let loop ((s :: int (- (string-length dna) len)))
      (let* ((key (string->symbol (dna:substring s (+ s len))))
	     (cnt (hash-table-ref/default table key 0)))
	(hash-table-set! table key (+ cnt 1)))
      (if (> s 0)
	  (loop (- s 1))))
    table))

(define (write-freqs table)
  (let* ((content (hash-table->alist table))
	 (total (exact->inexact (apply + (map cdr content)))))
    (for-each
     (lambda (a)
       (format #t "~a ~,3f~%"
	       (car a)
	       (* 100 (/ (cdr a) total))))
     (sort content (lambda (a b) (> (cdr a) (cdr b)))))))

(define (write-one-freq table key)
  (let ((cnt (hash-table-ref/default table key 0)))
    (format #t "~a\t~a~%" cnt key)))

(define dna
  (let ((in :: input-port (current-input-port))
	(sb (java.lang.StringBuilder)))
    ;; Skip to ">THREE ..."
    (do ()
	((let ((line (in:readLine)))
	   (or (eq? line #!null) (line:startsWith ">THREE")))))
    (let loop ()
      (let ((line (in:readLine)))
	(cond ((not (eq? line #!null))
	       (sb:append line)
	       (loop)))))
    ((sb:toString):toUpperCase)))

;; 1-nucleotide counts:
(write-freqs (all-counts 1 dna))
(newline)

;; 2-nucleotide counts:
(write-freqs (all-counts 2 dna))
(newline)

;; Specific sequences:
(for-each (lambda (seq)
	    (write-one-freq (all-counts (string-length seq) dna)
			    (string->symbol seq)))
	  '("GGT" "GGTA" "GGTATT" "GGTATTTTAATT" "GGTATTTTAATTTATAGT"))
