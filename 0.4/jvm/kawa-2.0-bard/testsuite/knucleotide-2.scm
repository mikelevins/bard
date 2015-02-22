;;   The Computer Language Shootout
;;   http://shootout.alioth.debian.org/

;; This is a Kawa implementation of the knucelotide benchmark.
;; This is is fairly low-level - it by-passes the Scheme APIs
;; to use pure Java libraries.  It does make use of
;; gnu.kawa.util.AbstractHashTable, but it sort-of cheats by getting
;; down into implementation internals.
;;
;; Original Kawa version based on mzscheme version,
;; with some bits based on Java version contributed by James McIlree.

(define-simple-class StrIntNode (java.util.Map$Entry)
  (next ::StrIntNode)
  (hash :: int)
  (key ::java.lang.String)
  (count ::int)
  ((getKey) key)
  ((getValue) ::object count))

(define (make-node key::string hash::int count::int) ::StrIntNode
   (let ((n (StrIntNode)))
     (set! n:key key)
     (set! n:hash hash)
     (set! n:count count)
     n))

(define-simple-class StrIntHashTable (gnu.kawa.util.AbstractHashTable)
  ((getEntryHashCode  entry)  (as StrIntNode entry):hash)
  ((getEntryNext entry) (as StrIntNode entry):next)
  ((setEntryNext entry next) (set! (as StrIntNode entry):next next))
  ((allocEntries n) (StrIntNode[] size: n))
  ((makeEntry key hash value) (make-node key hash value))
  ;; FIXME this code should be made into library code.
  ((toNodeArray) ::StrIntNode[]
   (let* ((arr ::StrIntNode[] table)
	  (length arr:length)
	  (n ::int ((this):size))
	  (result (StrIntNode[] length: n))
	  (j ::int 0))
     (do ((i ::int (- length 1) (- i 1)))
	 ((< i 0) result)
       (do ((node ::StrIntNode (table i)
		  node:next))
	   ((eq? node #!null) #!void)
	 (set! (result j) node)
	 (set! j (+ j 1)))))))

(define (all-counts (len :: int) (dna :: java.lang.String))
  (let ((table (StrIntHashTable)))
    (let loop ((s :: int (- (string-length dna) len)))
      (let* ((key (dna:substring s (+ s len)))
	     (node ::StrIntNode (table:getNode key)))
	(if (eq? node #!null)
	    ;; Non-optimal - requires recalculating hashCode.
	    (table:put key (key:hashCode) 1)
	    (set! node:count (+ node:count 1))))
      (if (> s 0)
	  (loop (- s 1))))
    table))

(define node-comparator ::java.util.Comparator
  (object (java.util.Comparator)
         ((compare o1 o2) ::int
          (let ((v1 ::int (as StrIntNode o1):count)
                (v2 ::int (as StrIntNode o2):count))
            (cond ((> v1 v2) -1)
                  ((< v1 v2) 1)
                  (else 0))))
         ((equals o)::boolean (eq? o (this)))))

(define (write-freqs table::StrIntHashTable) ::void
  (let* ((content (table:toNodeArray))
        (size content:length)
        (total ::double
               (let ((sum ::int 0))
                 (do ((i ::int 0 (+ i 1)))
                     ((>= i size) sum)
                   (set! sum (+ sum (content i):count)))
                 sum)))

    (java.util.Arrays:sort content node-comparator)
    (do ((i ::int 0 (+ i 1)))
       ((>= i size))
      (let ((a (content i)))
       (format #t "~a ~,3f~%"
               a:key
               (* 100 (/ (as double a:count) total)))))))

(define (write-one-freq table::StrIntHashTable key::string)
  (let* ((node ::StrIntNode (table:getNode key))
	 (cnt (if (eq? node #!null) 0 node:count)))
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
			    seq))
	  '("GGT" "GGTA" "GGTATT" "GGTATTTTAATT" "GGTATTTTAATTTATAGT"))
