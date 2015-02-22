(module-name <kawa.lib.kawa.regex>)
(require <kawa.lib.std_syntax>)
(export regex-quote regex-match? regex-match regex-match-positions
	regex-split regex-replace regex-replace*)

(define (regex-quote (str :: string)) :: java.lang.String
  (java.util.regex.Pattern:quote str))

(define (regex-match? re (str :: string)
		      #!optional (start :: int 0) (end :: int (str:length)))
  (let* ((rex :: regex (if (instance? re regex) re (regex (re:toString))))
	 (matcher (rex:matcher str)))
    (matcher:region start end)
    (matcher:find)))

(define (regex-match re (str :: string)
		      #!optional (start :: int 0) (end :: int (str:length)))
  (let* ((rex :: regex (if (instance? re regex) re (regex (re:toString))))
	 (matcher (rex:matcher str)))
    (matcher:region start end)
    (if (matcher:find)
	(let loop ((igroup :: int (matcher:groupCount))
		   (r '()))
	  (if (< igroup 0) r
	      (loop (- igroup 1)
		    (cons
		     (let ((start (matcher:start igroup)))
		       (if (< start 0) #f
			   (str:subSequence start (matcher:end igroup))))
		     r))))
	#f)))

(define (regex-match-positions re (str :: string)
				#!optional (start :: int 0) (end :: int (str:length)))
  (let* ((rex :: regex (if (instance? re regex) re (regex (re:toString))))
	 (matcher (rex:matcher str)))
    (matcher:region start end)
    (if (matcher:find)
	(let loop ((igroup :: int (matcher:groupCount))
		   (r '()))
	  (if (< igroup 0) r
	      (loop (- igroup 1)
		    (cons
		     (let ((start (matcher:start igroup)))
		       (if (< start 0) #f
			   (cons start (matcher:end igroup))))
		     r))))
	#f)))

(define (regex-split re (str :: string))
  (let* ((rex :: regex (if (instance? re regex) re (regex (re:toString)))) 
	 (parts (rex:split str -1))
         (plen parts:length)
         (rlist (gnu.lists.LList:makeList parts 0)))
    ;; Work around a Java 8 change.
    (if (and (> plen 1)
             (equal? (parts (- plen 1)) "")
             (not (equal? (parts 0) ""))
             ((rex:matcher ""):matches))
        (cons "" rlist)
        rlist)))

(define (regex-replace re (str :: string) repl) :: string
  (let* ((rex :: regex (if (instance? re regex) re (regex (re:toString))))
	 (matcher (rex:matcher str)))
    (if (matcher:find)
	(let ((sbuf :: java.lang.StringBuffer (java.lang.StringBuffer)))
	  (matcher:appendReplacement sbuf
				     (if (procedure? repl)
					 (java.util.regex.Matcher:quoteReplacement  (repl (matcher:group)))
					 repl))
	  (matcher:appendTail sbuf)
	  (sbuf:toString))
	str)))

(define (regex-replace* re (str :: string) repl) :: string
  (let* ((rex :: regex (if (instance? re regex) re (regex (re:toString))))
	 (matcher (rex:matcher str))
	 (sbuf :: java.lang.StringBuffer (java.lang.StringBuffer)))
    (if (procedure? repl)
	(let loop ()
	  (if (matcher:find)
	      (begin
		(matcher:appendReplacement sbuf
					   (java.util.regex.Matcher:quoteReplacement (repl (matcher:group))))

		loop))
	  (matcher:appendTail sbuf)
	  (sbuf:toString))
	(matcher:replaceAll repl))))

