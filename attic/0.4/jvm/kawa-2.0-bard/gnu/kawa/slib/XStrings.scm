(define (substring string start #!optional (length 2147483647))
  (if (or (eq? string #!void)
	  (eq? start #!void)
	  (eq? length #!void))
      #!void
      (let* ((s :: <java.lang.String> string)
	     (slen :: <int> (invoke s 'length))
	     (sindex :: <int> start)
	     (index :: <int> (- sindex 1))
	     (len :: <int> length)
	     (avail :: <int> (- slen index))
	     (rlen :: <int> (if (> len avail) avail len)))
	(invoke s 'substring index (+ index rlen)))))

(define (string-length string)
  (if (eq? string #!void)
      #!void
      (invoke (as <java.lang.String> string) 'length)))

	  
