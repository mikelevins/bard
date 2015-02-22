(format #t "[~a]~%"
        (* 10 (call/cc (lambda (k) (+ 2 (k 3))))))
;; Diagnostic: unreach3.scm:2:36: warning - unreachable procedure call
;; Diagnostic: unreach3.scm:2:41: note - this operand never finishes
;30
;; Output: [30]

(format #t "is-procedure: ~a~%"
        (procedure?
         (let () 
           (define aa 20)
           (define (foo) aa)
           (define (bar)
             (let loop ((arg 'bar))
               (foo)
               (not (loop (foo)))))
           bar)))
;; Diagnostic: unreach3.scm:16:16: warning - unreachable procedure call
;; Diagnostic: unreach3.scm:16:21: note - this operand never finishes
;; Output: is-procedure: #t

(define (test-catch)
  (let* ((x 0)
	 (y (catch 'key
		   (lambda ()
		     (set! x 2)
		     (throw 'key 10)
		     (set! x 1000))
		   (lambda (key arg)
		     (set! x (* x arg))
		     (+ x 10)))))
    (list x y)))
;; Diagnostic: unreach3.scm:28:8: warning - unreachable code
(format #t "test-catch: ~s~%"
        (test-catch))
;; Output: test-catch: (20 30)

;; Savannah bug #32678: set! and endless loop
(define (foo-savannah-32678 x)
  (let ((fail 0)
	(result #!null))
    (if (instance? x pair)
	(set! result (do () (#f)))
	(set! fail -1))
    (if (= fail 0) 
	result 
	#f)))
(format #t "foo-savannah-32678: ~w~%" (foo-savannah-32678 123))
;; Diagnostic: unreach3.scm:43:15: warning - 'result' can never be set because expression never finishes
;; Output: foo-savannah-32678: #f
