(define (current-readtable) :: <readtable>
  (invoke-static <readtable> 'getCurrent))

;; Common Lisp has readtablep
(define (readtable? obj) :: <boolean>
  (instance? obj <readtable>))

#|
(define (steam->lexer (stream :: <input-port>))
  :: <gnu.text.Lexer>
  ;; Associate a property table with an InPort?
  ...)

(define (copy-readtable #!optional from-readtable to-readtable)
  ...)

(define (set-syntax-from-char
	 (from-char :: <char>) (to-char :: <char>)
	 #!optional
	 (from-readtable :: <readtable> (current-readtable))
	 (to-readtable :: <readtable>)) ;; FIXME default
  ...)
|#

(define (set-macro-character (char :: <char>)
			     function
			     #!optional
			     (non-terminating :: <boolean> #f)
			     (readtable :: <readtable> (current-readtable)))
  (invoke readtable 'set char
	  (make <gnu.kawa.lispexpr.ReaderMacro> function non-terminating)))

#|
(define (get-macro-character
	 (char :: <char>)
	 #!optional
	 (readtable :: <readtable> (current-readtable)))
  (let ((entry :: <gnu.kawa.lispexpr.ReadTableEntry>
	       (invoke readtable 'lookup char)))
    (if (instance? entry <gnu.kawa.lispexpr.ReaderMacro>)
	(let ((macro :: <gnu.kawa.lispexpr.ReaderMacro>
		     entry))
	  (values (invoke entry 'getProcedure)
		  (invoke entry 'isNonTerminating)))
	(let* ((kind :: <int> (invoke entry 'getKind))
	       (non-terminating :: <boolean>
				(= kind (static-field <readtable> 'NON_TERMINATING_MACRO))))
	  (if (or non-terminating
		  (= kind (static-field <readtable> 'TERMINATING_MACRO)))
	      (values
	       (lambda ((stream :: <input-port>) (char :: <char>))
		 (invoke entry 'read (stream->lexer stream) char 1))
	       non-terminating)
	      (values #f))))))
|#

(define (make-dispatch-macro-character
	 (char :: <char>)
	 #!optional
	 (non-terminating :: <boolean> #f)
	 (readtable :: <readtable> (current-readtable)))
  (invoke readtable 'set char
	  (make <gnu.kawa.lispexpr.ReaderDispatch> non-terminating)))

(define (set-dispatch-macro-character
	 (disp-char :: <char>) (sub-char :: <char>)
	 function
	 #!optional (readtable :: <readtable> (current-readtable)))
  (let ((entry :: <gnu.kawa.lispexpr.ReaderDispatch>
	       (invoke readtable 'lookup disp-char)))
    (invoke entry 'set sub-char
	    (make <gnu.kawa.lispexpr.ReaderDispatchMacro> function))))

(define (get-dispatch-macro-table
	 (disp-char :: <char>) (sub-char :: <char>)
	 #!optional (readtable :: <readtable> (current-readtable)))
  (let* ((disp-entry :: <gnu.kawa.lispexpr.ReaderDispatch>
		     (invoke readtable 'lookup disp-char))
	 (sub-entry (invoke disp-entry 'lookup sub-char)))
    (if (eq? sub-entry #!null) #f sub-entry)))

(define (define-reader-ctor (key ::gnu.mapping.SimpleSymbol) (proc ::procedure)
	 #!optional (readtable ::readtable (current-readtable)))
  (*:putReaderCtor readtable key:name proc))
