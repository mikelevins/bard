#| Kawa
   The Computer Language Benchmarks Game
   http://shootout.alioth.debian.org/
   Contributed by Per Bothner
   Based on Java 6 server #4 version
   contributed by Razii, idea taken from Elliott Hughes and Roger Millington
|#

(import (srfi :69 basic-hash-tables))
(define replacements (alist->hash-table
	  '(("W" . "(a|t)")
	    ("Y" . "(c|t)")
	    ("K" . "(g|t)")
	    ("M" . "(a|c)")
	    ("S" . "(c|g)")
	    ("R" . "(a|g)")
	    ("B" . "(c|g|t)")
	    ("D" . "(a|g|t)")
	    ("V" . "(a|c|g)")
	    ("H" . "(a|c|t)")
	    ("N" . "(a|c|g|t)"))))


(define-syntax rewrite
  (syntax-rules ()
    ((rewrite pattern original replace)
     (let* ((matcher (pattern:matcher original))
	    (destination (java.lang.StringBuffer (original:length))))
       (do () ((not (matcher:find)))
	 (matcher:appendReplacement destination "")
	 (replace matcher destination))
       (matcher:appendTail destination)
       (destination:toString)))))

(define variants
  (java.lang.String[]
		   "agggtaaa|tttaccct"
		   "[cgt]gggtaaa|tttaccc[acg]"
		   "a[act]ggtaaa|tttacc[agt]t"
		   "ag[act]gtaaa|tttac[agt]ct"
		   "agg[act]taaa|ttta[agt]cct"
		   "aggg[acg]aaa|ttt[cgt]ccct"
		   "agggt[cgt]aa|tt[acg]accct"
		   "agggta[cgt]a|t[acg]taccct"
		   "agggtaa[cgt]|[acg]ttaccct"))

(define (regexdna (in :: java.io.InputStream))
  (let* ((r (java.io.InputStreamReader in "ISO-8859-1"))
	 (sb :: java.lang.StringBuilder
	  (let ((cbuf (char[] length: 16384))
		(sb (java.lang.StringBuilder 5100000)))
	    (let loop ()
	      (let ((chars-read (r:read cbuf)))
		(cond ((>= chars-read 0)
		       (sb:append cbuf 0 chars-read)
		       (loop)))))
	    sb))
	 (initial-length (sb:length))
	 (sequence :: string
		   (rewrite #/>.*\n|\n/ sb
			   (lambda (matcher destination) #!void)))
	 (code-length (sequence:length))
	 (nvariants variants:length))
    (do ((i :: int 0 (+ i 1)))
	((>= i nvariants))
      (let* ((count :: int 0)
	     (variant (variants i))
	     (m ((java.util.regex.Pattern:compile variant):matcher sequence)))
	(do ()
	    ((not (m:find)))
	  (set! count (+ count 1)))
	(format #t "~a ~d~%" variant count)))

    (set! sequence
	  (rewrite #/[WYKMSRBDVHN]/ sequence
		   (lambda (matcher destination)
		     (destination:append (replacements:get(matcher:group 0))))))

    (format #t "~%~d~%~d~%~d~%"
	    initial-length code-length (sequence:length))))

(regexdna java.lang.System:in)
