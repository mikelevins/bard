;;; progfun.scm -- functions dealing with BRL programs
;;; Copyright (C) 1999, 2000  Bruce R. Lewis and Eaton Vance Management
;;; See the file COPYING for license terms.

;;;;; Part I: NAL
;;;
;;; nal - named-argument lambda
;;; A nal is a list whose first element is a procedure,
;;; and whose remaining elements are symbols naming the
;;; second and subsequent arguments to the procedure.
;;; The first argument is brl-context, explained later.
;;;
#|

(define brl-nal-proc car)
(define brl-nal-args cdr)

;
;  Take a list of expressions (including the inputs expr)
;  and create a nal from it.
;

(define (brl-make-nal l)
  (let ((ins (brl-template-inputs l)))
    (cons (eval
	   (make <pair-with-position>
	     (as <pair-with-position> l)
	     'lambda
	     (list (cons 'brl-context ins)
		   `(letrec-syntax
			((brl-out
			  (syntax-rules (brl-content-type!
					 brl-http-header!
					 brl-set-cont!
					 brl-set-endproc!
					 brl-set-outport!
					 define define-syntax
					 set! set-car! set-cdr!
					 silent sql-statement-close
					 string-fill! string-set!
					 vector-fill! vector-set!
					 write)
			    ((brl-out (brl-content-type! expr ...))
			     (brl-content-type! expr ...))
			    ((brl-out (brl-http-header! expr ...))
			     (brl-http-header! expr ...))
			    ((brl-out (brl-set-cont! expr ...))
			     (brl-set-cont! expr ...))
			    ((brl-out (brl-set-endproc! expr ...))
			     (brl-set-endproc! expr ...))
			    ((brl-out (brl-set-outport! expr ...))
			     (brl-set-outport! expr ...))
			    ((brl-out (define expr ...)) (define expr ...))
			    ((brl-out (define-syntax expr ...))
			     (define-syntax expr ...))
			    ((brl-out (set! expr ...)) (set! expr ...))
			    ((brl-out (set! expr ...)) (set! expr ...))
			    ((brl-out (set-car! expr ...)) (set-car! expr ...))
			    ((brl-out (set-cdr! expr ...)) (set-cdr! expr ...))
			    ((brl-out (silent expr ...)) (begin expr ...))
			    ((brl-out (sql-statement-close expr ...))
			     (sql-statement-close expr ...))
			    ((brl-out (string-fill! expr ...))
			     (string-fill! expr ...))
			    ((brl-out (string-set! expr ...))
			     (string-set! expr ...))
			    ((brl-out (vector-fill! expr ...))
			     (vector-fill! expr ...))
			    ((brl-out (vector-set! expr ...))
			     (vector-set! expr ...))
			    ((brl-out (write expr ...)) (write expr ...))
			    ((brl-out expr) (display expr (brl-context-outport
							   brl-context)))
			    ((brl-out expr1 expr2 ...) (begin
							 (brl-out expr1)
							 (brl-out expr2 ...)))))
			 (brl
			  (syntax-rules ()
			    ((brl expr ...)
			     (begin (brl-out expr ...) ""))))
			 (brl-when
			  (syntax-rules ()
			    ((brl-when pred expr ...)
			     (if pred (brl expr ...) ""))))
			 (brl-unless
			  (syntax-rules ()
			    ((brl-unless pred expr ...)
			     (if pred "" (brl expr ...)))))
			 (paste
			  (syntax-rules ()
			    ((paste relative-uri)
			     (brl-paste brl-context relative-uri))))
			 (cgi
			  (syntax-rules ()
			    ((cgi var) ((; constant-fold ;FIXME
					 brl-sv-req-string-retriever
					 (quote var))
					(brl-context-sv-req brl-context))))))
		      ,(cons 'brl (brl-template-inputs-delete! l l))))))
	  ins)))
|#

(define-syntax brl
  (syntax-rules () ((brl expr ...) (values-append expr ...))))

#|

;;; brl-context - a list with at least two elements

(define brl-context-outport car)	; an output port
(define brl-context-ctin cadr)		; a continuation
(define brl-context-endproc caddr)	; a procedure or #f
; optional elements
(define brl-context-bindings cadddr)	; list of input symbols and values
(define (brl-context-sv c) (list-ref c 4)) ; Servlet
(define (brl-context-sv-req c) (list-ref c 5)) ; Servlet HTTP request
(define (brl-context-sv-rsp c) (list-ref c 6)) ; Servlet HTTP response
(define (brl-context-content-type c) (list-ref c 7)) ; Servlet HTTP response

; deprecated
(define (brl-context-type c)
  (if (instance? (list-ref c 4) <javax.servlet.Servlet>)
      'servlet
      'non-servlet))

(define brl-set-outport! set-car!)
(define (brl-continue c) ((brl-context-ctin c)))
(define (brl-set-cont! c ctin) (set-car! (cdr c) ctin))
(define (brl-set-endproc! c proc) (set-car! (cddr c) proc))
(define (brl-content-type! c type)
  (set-car! (cdddr (cddddr c)) type))

; Add to the existing endproc for a BRL context
(define (brl-prepend-endproc! c newproc)
  (let ((oldproc (brl-context-endproc c)))
    (brl-set-endproc!
     c
     (if (procedure? oldproc)
	 (lambda () (newproc) (oldproc))
	 newproc))))

; Find a binding value
(define (brl-binding-get ctxt var)
  (let loop ((input-list (brl-context-bindings ctxt)))
    (if (null? input-list)
	#f
	(if (eq? var (caar input-list))
	    (cdar input-list)
	    (loop (cdr input-list))))))
|#

;;;;; Part II: functions to read BRL template files

#|
(define brl-read (make <gnu.brl.read>))
(define brl-readall (make <gnu.brl.readall>))
|#

;
; A BRL template may have an expr (inputs a b c ...) to ease compilation
; This is never the 1st exp in the template.
;

#|
(define (brl-template-inputs l)
  (if (null? l)
      '()
      (if (and (list? (car l))
	       (eq? 'inputs (caar l)))
	  (cdar l)
	  (brl-template-inputs (cdr l)))))

(define (brl-template-inputs-delete! l start)
  (letrec ((delete-subsequent
	    (lambda (lst)
	      (if (or (not (pair? lst))
		      (not (pair? (cdr lst))))
		  start
		  (if (and (pair? (cadr lst))
			   (eq? 'inputs (caadr lst)))
		      (begin
			(set-cdr! lst (cddr lst))
			start)
		      (delete-subsequent (cdr lst)))))))
    (cond
     ((null? l) l)
     ((and (pair? l) (pair? (car l)) (eq? 'inputs (caar l)))
      (cdr l))
     (else (delete-subsequent l)))))

(define (brl-read-nal p)
  (brl-make-nal (brl-readall p)))

(define (brl-load fname)
  (call-with-input-file fname brl-read-nal))

(define (brl-result nal args partial-context)
  (let ((full-context (cons #f (cons #f (cons #f partial-context)))))
    (call-with-output-string
     (lambda (p)
       (try-finally
	(call-with-current-continuation
	 (lambda (ctin)
	   (brl-set-outport! full-context p)
	   (brl-set-cont! full-context ctin)
	   (apply (car nal) (cons full-context args))))
	(let ((endproc (brl-context-endproc full-context)))
	  (if (procedure? endproc) (endproc))))))))

;;;;; Part III: binding sets

(define (brl-binding-make name val) (cons name val))

(define (brl-binding-add bset b)
  (let ((existing (assq (car b) bset)))
    (if existing
	(begin
	  (set-cdr! existing
		    ((if (list? (cdr existing))
			 cons
			 list) (cdr b) (cdr existing)))
	  bset)
	(cons b bset))))

(define (brl-bindings bset blist)
  (if (null? blist)
      bset
      (brl-bindings (brl-binding-add bset (car blist))
		    (cdr blist))))

(define (brl-apply nal bset partial-context)
  (brl-result
   nal
   (map (lambda (sym)
	  (let ((binding (assq sym bset)))
	    (if binding
		(cdr binding)
		'())))
	(cdr nal))
   partial-context))
|#

;
; Generic hash table interface
;

(define (brl-hash)
  (make <java.util.Hashtable>))

(define (brl-hash? obj)
  (instance? obj <java.util.Dictionary>))

(define (brl-hash-size hh :: <java.util.Dictionary>)
  (make <integer> (invoke hh 'size)))

(define (brl-hash-put! hh :: <java.util.Dictionary> key val)
  (invoke hh 'put key val))

(define (brl-hash-get hh :: <java.util.Dictionary> key)
  (let ((result (invoke hh 'get key)))
    (if (eq? #!null result)
	#f				; as assoc does
	result)))

(define (brl-hash-remove! hh :: <java.util.Dictionary> key)
  (invoke hh 'remove key))

(define (brl-hash-keys hh :: <java.util.Dictionary>)
  (letrec ((enum (lambda (ee :: <java.util.Enumeration>)
		   (if (invoke ee 'hasMoreElements)
		       (let ((nxt (invoke ee 'nextElement)))
			 (cons nxt (enum ee)))
		       '()))))
    (enum (invoke hh 'keys))))

(define (brl-hash-contains-key? hh :: <java.util.Hashtable> key)
  (invoke hh 'containsKey key))

#|
;
; Cache of NALs, implemented as a hash table
; with filenames as keys, and (cons NAL file-modtime) as values
;

(define brl-nal-cache (brl-hash))

; Get nal from cache, creating if needed

(define (brl-nal-cache-get fname)
  (let ((cached (brl-hash-get brl-nal-cache fname))
	(new-modtime (file-last-modified fname)))
    (if (or (not cached)
	    (not (= new-modtime (cdr cached))))
	(let ((new-nal (brl-load fname)))
	  (brl-hash-put! brl-nal-cache fname (cons new-nal new-modtime))
	  new-nal)
	(car cached))))

(define (brl-handle-2.23 fname blist partial-context)
  (let ((nal (brl-nal-cache-get fname)))
    (brl-apply
     nal
     (brl-bindings '() blist)
     (cons blist partial-context))))
|#

#|
(define (brl-paste ctxt relative-uri)
  (let ((sv :: <gnu.brl.brlsv> (brl-context-sv ctxt))
	(req :: <javax.servlet.http.HttpServletRequest>
	     (brl-context-sv-req ctxt)))
    (let ((nal
	   (brl-nal-cache-get
	    (invoke sv 'getPertinentFile
		    req
		    (invoke sv 'getRelativeURI req relative-uri)))))
      (if (null? (cdr nal))
	  ((car nal) ctxt)
	  (apply (car nal)
		 (cons ctxt (map (lambda (var)
				   (or (brl-binding-get ctxt var) '()))
				 (cdr nal))))))))
|#

#|
; Backwards compatibility
(define (brl-handle-request fname blist . partial-context)
  (brl-handle-2.23 fname blist partial-context))
|#

(define (file-last-modified fname)
  (kawa-convert
   ((primitive-virtual-method <java.io.File>
			      "lastModified" <long> ())
    ((primitive-constructor <java.io.File> (<String>))
     fname))))

; Misc

#|
(define (brl-implementation-version)
  (make <string> (as <java.lang.String>
		     (static-field <gnu.brl.Version> 'release_string))))
|#

(define brl-random (make <gnu.brl.random>))

(define brl-typeable-chars
  ; Letters/numbers, excepting o, 0, 1, l
  (list->vector (string->list "abcdefghijkmnpqrstuvwxyz23456789")))

(define brl-typeable-count (vector-length brl-typeable-chars))

(define (brl-random-typeable len)
  (let ((retval (make-string len)))
    (let loop ((i 0))
      (if (>= i len)
	  retval
	  (begin
	    (string-set!
	     retval i (vector-ref brl-typeable-chars
				  (brl-random brl-typeable-count)))
	    (loop (+ 1 i)))))))
