;;; stringfun.scm -- BRL string functions
;;; Copyright (C) 1999, 2000  Bruce R. Lewis and Eaton Vance Management
;;; See the file COPYING for license terms.

; Any object(s) --> string

(define (brl-string obj . etc)
  (if (null? etc)
      (cond ((string? obj) obj)
	    ((number? obj) (number->string obj))
	    (else (call-with-output-string
		   (lambda (p) (display obj p)))))
      (apply string-append (map brl-string (cons obj etc)))))

;;
;; Text formatting
;;

(define (brl-now) ((primitive-constructor <java.util.Date> ())))

(define (brl-simple-date-format pattern)
  ((primitive-constructor <java.text.SimpleDateFormat>
			  (<String>))
   pattern))

(define brl-SimpleDateFormat ; deprecated
  brl-simple-date-format)

(define (brl-simple-date-formatter pattern . null-value-list)
  (let ((sdf (brl-simple-date-format pattern)))
    (if (pair? null-value-list)
	(lambda (date)
	  (if (null? date)
	      (car null-value-list)
	      (if (string? date)
		  ((primitive-virtual-method
		    <java.text.SimpleDateFormat> "parse"
		    <java.util.Date>
		    (<String>)) sdf date)
		  ((primitive-constructor <string> (<java.lang.String>))
		   ((primitive-virtual-method
		     <java.text.SimpleDateFormat> "format"
		     <java.lang.String>
		     (<java.util.Date>)) sdf date)))))
	(lambda (date)
	  (if (string? date)
	      ((primitive-virtual-method
		<java.text.SimpleDateFormat> "parse"
		<java.util.Date>
		(<String>)) sdf date)
	      ((primitive-constructor <string> (<java.lang.String>))
	       ((primitive-virtual-method
		 <java.text.SimpleDateFormat> "format"
		 <java.lang.String>
		 (<java.util.Date>)) sdf date)))))))

(define brl-SimpleDateFormatter ; deprecated
  brl-simple-date-formatter)

(define (brl-decimal-format pattern)
  ((primitive-constructor <java.text.DecimalFormat>
			  (<String>))
   pattern))

(define brl-DecimalFormat ; deprecated
  brl-decimal-format)

(define (brl-decimal-formatter pattern . null-value-list)
  (let ((sdf (brl-decimal-format pattern)))
    (if (pair? null-value-list)
	(lambda (num)
	  (if (null? num)
	      (car null-value-list)
	      ((primitive-constructor <string> (<java.lang.String>))
	       ((primitive-virtual-method
		 <java.text.DecimalFormat> "format"
		 <java.lang.String>
		 (<double>)) sdf num))))
	(lambda (num)
	  ((primitive-constructor <string> (<java.lang.String>))
	   ((primitive-virtual-method
	     <java.text.DecimalFormat> "format"
	     <java.lang.String>
	     (<double>)) sdf num))))))

(define brl-DecimalFormatter ; deprecated
  brl-decimal-formatter)

(define (brl-format obj pattern)
  ((if (number? obj)
       (brl-decimal-formatter pattern)
       (brl-simple-date-formatter pattern))
   obj))

;;;
;;; Various escapes (HTML, LaTeX, etc.)
;;;

(define (brl-string-escaper esc)
  (let ((spec (brl-char-escape-spec esc)))
    (lambda (str) (brl-string-escape str spec))))

(define (brl-string-needs-escape? str esc)
  (if (string? str)
      (let ((len (string-length str)))
	(let loop ((i 0))
	  (if (= i len)
	      #f
	      (let ((c (string-ref str i)))
		(if (and (char>=? c (car esc))
			 (char<=? c (cadr esc)))
		    #t
		    (loop (+ 1 i)))))))
      #f))

(define (brl-string-escape str esc)
  (if (brl-string-needs-escape? str esc)
      (list->string
       (reverse
	(let ((len (string-length str)))
	  (let loop ((i 0)
		     (li '()))
	    (if (= i len)
		li
		(loop (+ 1 i)
		      (let ((c (string-ref str i)))
			(if (and (char>=? c (car esc))
				 (char<=? c (cadr esc)))
			    (let ((li2 (vector-ref
					(caddr esc)
					(- (char->integer c)
					   (char->integer (car esc))))))
			      (if li2
				  (append li2 li)
				  (cons c li)))
			    (cons c li)))))))))
      str))

(define (brl-char-escape-spec speclist)
  (let ((minchar (caar speclist))
	(maxchar (caar speclist)))
    (let loop ((li (cdr speclist)))
      (if (not (null? li))
	  (begin
	    (let ((testchar (caar li)))
	      (if (char<? testchar minchar)
		  (set! minchar testchar))
	      (if (char>? testchar maxchar)
		  (set! maxchar testchar)))
	    (loop (cdr li)))))
    (list
     minchar
     maxchar
     (let ((specv (make-vector (+ 1 (- (char->integer maxchar)
				       (char->integer minchar))) #f)))
      (map (lambda (specpair)
	     (vector-set! specv
			  (- (char->integer (car specpair))
			     (char->integer minchar))
			  (reverse (string->list (cdr specpair)))))
	   speclist)
      specv))))

; Kawa needs this to clarify error messages
(define-syntax brl-name-proc
  (syntax-rules ()
    ((brl-name-proc name)
     (invoke (as <procedure> name) 'setName (quote name)))))

(define brl-html-escape (brl-string-escaper '((#\< . "&lt;")
					      (#\> . "&gt;")
					      (#\" . "&quot;")
					      (#\& . "&amp;"))))

(define brl-scheme-escape (brl-string-escaper '((#\\ . "\\\\")
						(#\" . "\\\""))))

(define brl-csv-escape (brl-string-escaper '((#\" . "\"\""))))

(define brl-sql-escape (brl-string-escaper '((#\' . "''"))))

(define brl-mysql-escape (brl-string-escaper '((#\' . "''")
					       (#\\ . "\\\\"))))

(define brl-msft-escape (brl-string-escaper '((#\202 . ",")
					      (#\203 . "f")
					      (#\204 . ",,")
					      (#\205 . "...")
					      (#\206 . "+")
					      (#\207 . "++")
					      (#\210 . "^")
					      (#\211 . "%o")
					      (#\212 . "S")
					      (#\213 . "<")
					      (#\214 . "OE")
					      (#\216 . "Z")
					      (#\221 . "`")
					      (#\222 . "'")
					      (#\223 . "\"")
					      (#\224 . "\"")
					      (#\225 . "*")
					      (#\226 . "--"))))

(define (brl-name-string-procs)
  (brl-name-proc brl-html-escape)
  (brl-name-proc brl-scheme-escape)
  (brl-name-proc brl-csv-escape)
  (brl-name-proc brl-sql-escape)
  (brl-name-proc brl-mysql-escape)
  (brl-name-proc brl-msft-escape))

; Make a quoted string suitable for a SQL statement
(define (brl-sql-string obj)
  (if (null? obj)
      "NULL"
      (string-append "'" (brl-sql-escape (brl-string obj)) "'")))

; Make a quoted string suitable for a MySQL statement
(define (brl-mysql-string obj)
  (if (null? obj)
      "NULL"
      (string-append "'" (brl-mysql-escape (brl-string obj)) "'")))

; Make a string representing a valid (or harmless) number in SQL
; Harmless: a valid Scheme number that breaks SQL but no security breach
; Empty or otherwise non-number strings will yield "NULL".
(define (brl-sql-number obj)
  (if (number? obj)
      (number->string obj)
      (let ((num (and (string? obj) (string->number obj))))
	(if num
	    (number->string num)
	    "NULL"))))

(define brl-latex-escape (brl-string-escaper '((#\\ . "\\\\")
					       (#\~ . "\\~")
					       (#\# . "\\#")
					       (#\$ . "\\$")
					       (#\% . "\\%")
					       (#\^ . "\\^")
					       (#\& . "\\&")
					       (#\{ . "\\{")
					       (#\} . "\\}")
					       (#\_ . "\\_"))))

(define (brl-newline-escaper str)
  (brl-string-escaper `((#\newline . ,str))))

;;; URL argument escaping

(define (brl-url-escape str)
  ((primitive-constructor <string> (<java.lang.String>))
   ((primitive-static-method <java.net.URLEncoder> "encode"
			     <String> (<String>))
    str)))

;;; comma-separated-value (.csv) escaping

(define brl-csv-date (brl-simple-date-formatter "yyyy-MM-dd HH:mm:ss"))

(define (brl-csv-string obj)
  (cond ((string? obj)
	 (string-append "\"" (brl-csv-escape obj) "\""))
	((null? obj)
	 "")
	((instance? obj <java.util.Date>)
	 (brl-csv-date obj))
	(else
	 (brl-string obj))))

;;
;; misc
;;

(define (brl-nonblank? obj)
  (and (not (or (null? obj) (equal? "" obj)))
       obj))

(define (brl-blank? obj)
  (or (null? obj) (equal? "" obj) (not obj)))

(define (brl-any-blank? . objs)
  (if (null? objs)
      #f
      (or (brl-blank? (car objs))
	  (apply brl-any-blank? (cdr objs)))))

(define (brl-any-nonblank? . objs)
  (if (null? objs)
      #f
      (or (brl-nonblank? (car objs))
	  (apply brl-any-nonblank? (cdr objs)))))

(define (brl-all-blank? . objs)
  (if (null? objs)
      #t
      (and (brl-blank? (car objs))
	   (apply brl-all-blank? (cdr objs)))))

(define (brl-all-nonblank? . objs)
  (if (null? objs)
      #t
      (and (brl-nonblank? (car objs))
	   (apply brl-all-nonblank? (cdr objs)))))

; HTML inputs may be single values or lists.  Use brl-list to force to list.
(define (brl-list obj)
  (if (list? obj) obj (list obj)))

(define (brl-nonblanks lst)
  (if (null? lst)
      '()
      (if (brl-nonblank? (car lst))
	  (cons (car lst) (brl-nonblanks (cdr lst)))
	  (brl-nonblanks (cdr lst)))))

(define (brl-list-separate sep lst)
  (if (or (null? lst) (null? (cdr lst)))
      lst
      (cons (car lst)
	    (cons sep (brl-list-separate sep (cdr lst))))))

(define (brl-jclass obj)
   ((primitive-virtual-method <java.lang.Class>
			      "getName" <String> ())
    ((primitive-virtual-method <java.lang.Object>
			       "getClass" <java.lang.Class> ())
     obj)))

(define (brl-string-join sep objs)
  (apply string-append (brl-list-separate (brl-string sep)
					  (map brl-string objs))))

(define (brl-starts-with? prefix obj)
  (invoke (as <String> (invoke obj 'toString)) 'startsWith prefix))

(define (brl-ends-with? suffix obj)
  (invoke (as <String> (invoke obj 'toString)) 'endsWith suffix))

; quick and dirty hack to be deprecated after SRFI 13

(define (brl-trim-list charlist)
  (cond ((null? charlist) '())
	((char-whitespace? (car charlist)) (brl-trim-list (cdr charlist)))
	(else charlist)))

(define (brl-trim str)
  (let ((last (- (string-length str) 1)))
    (if (and (>= last 0)
	     (or (char-whitespace? (string-ref str 0))
		 (char-whitespace? (string-ref str last))))
	(let ((left (let loop ((i 0))
		      (if (or (= i last)
			      (not (char-whitespace? (string-ref str i))))
			  i
			  (loop (+ i 1))))))
	  (substring
	   str
	   left
	   (let loop ((i last))
	     (if (or (= i left)
		     (not (char-whitespace? (string-ref str i))))
		 (+ i 1)
		 (loop (- i 1))))))
	str)))

(define (brl-split sep str)
  (let ((lenstr (string-length str))
	(lensep (string-length sep)))
    (let loop ((curpos 0)
	       (piece-starts '(0))
	       (piece-ends '()))
      (if (= curpos lenstr)
	  (map (lambda (start end) (substring str start end))
	       (reverse piece-starts)
	       (reverse (cons lenstr piece-ends)))
	  (if (and (<= (+ curpos lensep) lenstr)
		   (string=? sep (substring str curpos (+ curpos lensep))))
	      (loop (+ curpos 1)
		    (cons (+ curpos lensep) piece-starts)
		    (cons curpos piece-ends))
	      (loop (+ curpos 1) piece-starts piece-ends))))))
