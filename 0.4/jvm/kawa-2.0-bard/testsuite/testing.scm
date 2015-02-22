;; Copyright 1996, 1997, 1998 Per Bothner.
;;
;; Usage:
;; (load "testing.scm")
;; (test-init "Miscellaneous" 2)
;; (test '(3 4 5 6) (lambda x x) 3 4 5 6)
;; (test '(a b c . d) 'dot '(a . (b . (c . d))))
;; (test-report)
;;
;; test-init:  The first argument is the name of the test.
;; A log is written to (string-append NAME ".log").
;; The second (optional) argument is the total number of tests;
;; at the end an error is written if the actual count does not match.
;;
;; test:  The first argument is the expected result.
;; The second argument is either a procecure applied to the remaining
;; arguments;  or it is a symbol (used when reporting), in which case
;; the third argument is matched against the first.
;; The resulting values are matched using equal?.
;;
;; section:  You can divide your tests into "sections" with the section
;; procedure.  The arguments of the previous section are displayed if any
;; errors are reported.
;;
;; test-report:  Called at end to print a summary.
;;
;; fail-expected:  If non-false, if means the following test is
;; expected to fail.  The actual value should be string explaining
;; the failure.  For example:
;; (set! fail-expected "sqrt of negative number not supported")
;; (test "+2.0i" number->string (sqrt -4))
;;
;; verbose:  If true, all tests are written to standard output,
;; not just to the log file.

(define verbose #f)

(define pass-count 0)
(define fail-count 0)
(define xfail-count 0)
(define xpass-count 0)

(define *log-file* #f)

(define test-name "<unknown>")

;;; Set this (to an explanatory string) if the next test is known to fail.
(define-variable fail-expected #f)

;;; The current section.
(define cur-section #f)
;;; The section when we last emitted a message.
(define last-section #f)

(define total-expected-count #f)

(define (test-init name #!optional total-count)
  (set! test-name name)
  (set! total-expected-count total-count)
  (set! *log-file* (open-output-file (string-append name ".log")))
  (display (string-append "%%%% Starting test " name) *log-file*)
  (newline *log-file*)
  (display (string-append "%%%% Starting test " name
			  "  (Writing full log to \"" name ".log\")"))
  (newline)
  (set! pass-count 0)
  (set! xpass-count 0)
  (set! fail-count 0)
  (set! xfail-count 0))

(define (display-section port)
  (display "SECTION" port)
  (do ((l cur-section (cdr l)))
      ((null? l) #f)
    (write-char #\Space port)
    (display (car l) port))
  (newline port))

(define (maybe-report-section)
  (and cur-section *log-file* (not (eq? cur-section last-section))
       (begin (display-section (current-output-port))
	      (set! last-section cur-section))))

(define (section . args)
  (set! cur-section args)
  (display-section (or *log-file* (current-output-port)))
  (set! last-section #f))

(define record-error (lambda (e) (set! errs (cons (list cur-section e) errs))))

(define (report-pass port fun args res)
  (display (if fail-expected "XPASS:" "PASS: ") port)
  (write (cons fun args) port)
  (display "  ==> " port)
  (write res port)
  (newline port))

(define (report-fail port fun args res expect)
  (display (cond ((string? fail-expected)
		  (string-append "XFAIL (" fail-expected "): "))
		 (fail-expected "XFAIL: ")
		 (else "FAIL: "))
	   port)
  (write (cons fun args) port)
  (display "  ==> " port)
  (write res port)
  (display " BUT EXPECTED " port)
  (write expect port)
  (newline port))

#|
(define-syntax test
  (syntax-rules ()
		((test expect fun . args)
		 (cons #t (test1 (source-filename) (source-line) expect fun . args)))))
|#

;; test.scm redefines + in one of its tests.  This causes problems when we
;; want to use '+ in the test function below.  The clean (future) solution
;; would be to put this file inside a module.  FIXME.

(define (test expect fun . args)
  ((lambda (res)
     (cond ((equal? expect res)
	    (if fail-expected
		(set! xpass-count (+ xpass-count 1))
		(set! pass-count (+ pass-count 1)))
	    (if *log-file*
		(report-pass *log-file* fun args res))
	    (cond ((or verbose fail-expected)
		   (maybe-report-section)
		   (report-pass (current-output-port) fun args res))))
	   (#t
	    (if fail-expected
		(set! xfail-count (+ xfail-count 1))
		(set! fail-count (+ fail-count 1)))
	    (if *log-file*
		(report-fail *log-file* fun args res expect))
	    (cond ((or verbose (not fail-expected))
		   (maybe-report-section)
		   (report-fail (current-output-port) fun args res expect)))))
     (set! fail-expected #f))
   (if (procedure? fun)
       (cond-expand (kawa
		     (try-catch
		      (apply fun args)
		      (ex <java.lang.Throwable>
			  ;; (invoke ex 'printStackTrace) ;; for DEBUGGING
			  ex)))
		    (else
		     (apply fun args)))
       (car args))))

(define (report-display value)
  (display value)
  (and *log-file* (display value *log-file*)))

(define (report-newline)
  (newline)
  (and *log-file* (newline *log-file*)))

(define (report1 value string)
  (cond ((> value 0)
	 (report-display string)
	 (report-display value)
	 (report-newline))))

(define (test-report)
  (report1 pass-count  "# of expected passes      ")
  (report1 xfail-count "# of expected failures    ")
  (report1 xpass-count  "# of unexpected successes ")
  (report1 fail-count  "# of unexpected failures  ")
  (if (and total-expected-count
	   (not (= total-expected-count
		   (+ pass-count xfail-count xpass-count fail-count))))
      (begin
	(report-display "*** Total number of tests should be: ")
	(report-display total-expected-count)
	(report-display ". ***")
	(report-newline)
	(report-display "*** Discrepancy indicates testsuite error or exceptions. ***")
	(report-newline)))
  (cond (*log-file*
	 (close-output-port *log-file*)
	 (set! *log-file* #f))))
