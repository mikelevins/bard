(test-init "Miscellaneous" 218)

;;; DSSSL spec example 11
(test '(3 4 5 6) (lambda x x) 3 4 5 6)
(test '(5 6) (lambda (x y #!rest z) z) 3 4 5 6)
(test '(3 4 5 i: 6 j: 1)
      (lambda ( x y #!optional z #!rest r #!key i (j 1))
	(list x y z i: i j: j))
      3 4 5 i: 6 i: 7)

;; Test for optional argument handling.
;; (Savannah bug #10613 was a bug in name scoping of default arguments.)
(define (test-opt-args a b c d)
  (define n 0)
  ;; We add a side-effect to check that default arguments are evaluated
  ;; at the correct time.
  (define (next-n) (set! n (+ 1 n)) n)
  (define (inner a
		 #!optional (b (list a b c d (next-n)))
		 (c (list a b c d (next-n)))
		 #!key (d (list a b c d (next-n))))
    (vector arg-a: a arg-b: b arg-c: c argd: d))
  (list inner1: (inner 'a2) n: (next-n)
	inner2: (inner 'a3 'b3 'c3 d: 'd3) n: (next-n)))
(test
 '(inner1:
   #(
     arg-a: a2
     arg-b: (a2 b1 c1 d1 1)
     arg-c: (a2 (a2 b1 c1 d1 1) c1 d1 2)
     argd:  (a2 (a2 b1 c1 d1 1) (a2 (a2 b1 c1 d1 1) c1 d1 2) d1 3))
   n: 4
   inner2:
   #(arg-a: a3 arg-b: b3 arg-c: c3 argd: d3)
   n: 5) 
 test-opt-args 'a1 'b1 'c1 'd1)

(test '(200 . 100)
      (lambda (x #!optional (y (* 2 x)) (p (lambda () (cons y x))))
	(p))
      100)

(test '(100 . 200)
      (lambda (x #!optional (y (* 2 x)))
	(cons x y))
      100)

(test #t keyword? 'foo:)
(test #f keyword? 'foo\:)
(test #t keyword? 'foo:)
(test #t keyword? foo:)
(test #f keyword? 'foo)
(test #f keyword? ':)
(test #t keyword? ||:)
(test #t keyword? (car '(a: b:)))
(test #f keyword? "bar")

;; This is Savannah bug #39059: Method keywords problem
(define (key-1 #!key (a "default a") (b "default b") (c "default c"))
  (list c a))
(test '("c" "a") 'key-1 (key-1 a: "a" b: "b" c: "c"))

(define key-2-counter 0)
(define (incr-key-2-counter)
  (set! key-2-counter (+ key-2-counter 1))
  key-2-counter)
(define (key-2 #!key (a "default a") (b (incr-key-2-counter)) (c "default c"))
  (list c a key-2-counter))
(test '("c" "a" 0) 'key-2a (key-2 a: "a" b: "b" c: "c"))
(test '("default c" "a" 1) 'key-2b (key-2 a: "a"))

;;; DSSSL spec example 44
(test "Argentina" keyword->string \Argentina:)
(test "foo" keyword->string foo:)
(test "" keyword->string ||:)
(test "a b c" keyword->string (string->keyword "a b c"))
(test foo: string->keyword "foo")
(test ||: string->keyword "")

;; Test keyword parameter with primitive type.
(define (fun-with-keys1 #!key (code ::int 400) (message ::string "brrp"))
   (format "code: ~a message: ~a." code message))
(test "code: 400 message: brrp." 'test-fun-with-keys1 (fun-with-keys1))
(test "code: 200 message: brrp." 'test-fun-with-keys1 (fun-with-keys1 code: 200))

(test "Hello" symbol->string 'H\x65;llo)

;;; DSSSL spec example 45
(test foobar: string->keyword "foobar")

(define-unit ft 12in)
(test 18in + 6in 1ft)

(test 5 call-with-values (lambda () (values 4 5)) (lambda (a b) b))
(test -1 call-with-values * -)
;; Test from: Joerg-Cyril.Hoehle@t-systems.com
(test '(() #!eof) call-with-values
      (lambda () (values '() '#!eof))
      (lambda (x y) (list x y)))

;;; This caused a spurious warning in earlier releases.
(test '(1 2 3) 'let (let ((x (lambda l l))) (x 1 2 3)))

;;; test old reader bugs 
(test '(b) cdr '(a .(b))) 
(test "foo" cdr '(a ."foo")) 
(test 'a car '(a #||#)) 

(define (try-vector-ref vec index)
  (try-catch (vector-ref vec index)
	     (ex <java.lang.IndexOutOfBoundsException>
		 "Bad array index")))

(test 3 try-vector-ref #(1 2 3) 2)
(test "Bad array index" try-vector-ref #(1 2 3) 10)

;; Extracted from bug reported by Joerg-Cyril.Hoehle@t-systems.com
(define (test-unary-minus)
  (- (char->integer #\0)))
(test -48 test-unary-minus)
(define (test-string->integer str start end)
  (and (< -1 start end (+ (string-length str) 1))
       (let loop ((pos start) (accum 0))
	 (cond
	  ((>= pos end) accum)
	  ((char-numeric? (string-ref str pos))
	   (loop (+ pos 1) (+ (char->integer (string-ref str pos))
			      (- (char->integer #\0)) (* 10 accum))))
	  (else #f)))))
(test 123 test-string->integer "123" 0 3)
(test 123 test-string->integer "123456" 0 3)
(test 23 test-string->integer "123456" 1 3)

(section "new-line handling")
;;; Test that #\return and #\newline are read robustly.

(define cr-test-string (string-copy "a \"bRLc\" dRklLXY"))
(do ((i 0 (+ i 1)))
    ((= i (string-length cr-test-string)) #t)
  (if (char=? #\R (string-ref cr-test-string i))
      (string-set! cr-test-string i #\Return))
  (if (char=? #\L (string-ref cr-test-string i))
      (string-set! cr-test-string i #\Linefeed)))
(call-with-input-string
 cr-test-string
 (lambda (iport ::input-port)
   (iport:setConvertCR #t)
   (test 1 input-port-column-number iport)
   (test 1 input-port-line-number iport)
   (test 'a read iport)
   (test "b\nc" read iport)
   (test 'd read iport)
   (test 'kl read iport)
   (test 'XY read iport)
   (test #!eof read iport)))

(call-with-input-string
 cr-test-string
 (lambda (iport)
   (test #\a read-char iport)
   (test #\Space read-char iport)
   (test #\" read-char iport)
   (test #\b read-char iport)
   (test #\Return peek-char iport)
   (test 5 input-port-column-number iport)
   (test 1 input-port-line-number iport)
   (test #\Return read-char iport)
   (test #\Linefeed read-char iport)
   (test #\c read-char iport)
   (test #\" read-char iport)
   (test #\Space read-char iport)
   (test #\d read-char iport)
   (test #\Return read-char iport)
   (test 3 input-port-line-number iport)
   (test 1 input-port-column-number iport)
   (test #\k read-char iport)
   (test #\l read-char iport)
   (test #\Linefeed read-char iport)
   (test #\X read-char iport)
   (test #\Y read-char iport)
   (test #!eof read-char iport)))

(define str-inport (open-input-string "(a . (b c . ())) 34"))
(test #t input-port? str-inport)
(test '(a b c) read str-inport)
(test 34 read str-inport)
(test #t eof-object? (peek-char str-inport))
(close-input-port str-inport)

(test "a/b (c d)" 'open-output-string
      (let ((q (open-output-string))
            (x '(a b c d)))
        (write (car x) q)
	(display "/" q)
        (write (cadr x) q)
        (write (cddr x) q)
        (get-output-string q)))

;;; From: Hallvard Traetteberg <Hallvard.Traetteberg@idi.ntnu.no>
;;; Triggered bug with try-finally nested in an expression.

(define (quote-keyword-values list)
  (if (null? list)
  list
  `(,(car list) ',(car (cdr list))
  . ,(quote-keyword-values (cdr (cdr list)))))
  )

(defmacro with-content (object-form . content)
  (let ((var-symbol (string->symbol (string-append "context-"
(symbol->string (car object-form)))))
    (object-form `(,(car object-form)
         . ,(quote-keyword-values (cdr object-form)))))
  `(fluid-let ((,var-symbol ,object-form))
   (let ((content (list . ,content)))
         (cons ,var-symbol content)))
  ))

(define (document) (list 'document))
(define (view #!key type)
  (list 'view type: type))

(test '((view type: text)) 'with-content
      (with-content (view type: text)))
(test '((document) ((view type: diagram)) ((view type: text))) 'with-content
      (with-content (document) (with-content (view type: diagram))
		    (with-content (view type: text))))

(test '("X" . "X:abc") 'synchronized
      (let* ((x "X")
	     (y "abc")
	     (z	(synchronized y
			      (set! y (string-append x ":" y))
			      (cons x y))))
	z))

(define *xx* 3)
(define (fluid-test *xx*)
  (fluid-let ((*xx* *xx*))
    (set! *xx* (+ 100 (twice-*xx*)))
    (set! *xx* (let ((*xx* *xx*))
		 (+ 100 *xx*)))
    *xx*))
(define (twice-*xx*) (* 2 *xx*))
(test '(206 . 3) 'fluid-let-1 (let ((res (fluid-test 10))) (cons res *xx*)))

(test 'bar 'fluid-let-2 (fluid-let ((flt (lambda () 'bar))) (flt)))

(section "closures")

(define (f1 a)
  (define (f2 b)
    (cons a b))
  (cons a f2))
(define f1-100 (f1 100))
(define f2-20 ((cdr f1-100) 20))
(test 100 'closure-f2-car (car f2-20))
(test 20 'closure-f2-cdr (cdr f2-20))

;; Here f4 should be optimized away.
(define (f3 a)
  (define (f4 b)
    (cons a b))
  (define (f5 c)
    (cons a c))
  (cons a f5))
(define f3-10 (f3 10))
(define f4-20 ((cdr f3-10) 20))
(test '(10 . 20) 'closure-f4-20 f4-20)

(define (f30 a)
  (define (f31 b)
    (cons a b))
  (define (f32 c)
    (cons a c))
  (list a f31 f32))
(define f30-10 (f30 10))
(define f31-20 ((cadr f30-10) 20))
(define f32-33 ((caddr f30-10) 33))
(test '(10 . 20) 'closure-f31-20 f31-20)
(test '(10 . 33) 'closure-f32-33 f32-33)

(define (f6 a)
  (define (f7 b)
    (define (f8 c)
      (define (f9 d)
        (list a b c d))
      (list a b c f9))
    (list a b f8))
  (list a f7))
(define f6-100 (f6 100))
(define f7-20 ((cadr f6-100) 20))
(define f8-10 ((caddr f7-20) 10))
(test '(100 20 10 2) 'closure-test3 ((cadddr f8-10) 2))

(define (f60 a)
  (define (x6 b) a)
  (define (f70 b)
    (define (x7 c) b)
    (define (f8 c)
      (define (x8 d) c)
      (define (f9 d)
        (list a b c d))
      (list a b c f9))
    (list a b f8))
  (list a f70))
(define f60-100 (f60 100)) 
(define f70-20 ((cadr f60-100) 20)) 
(define f80-10 ((caddr f70-20) 10)) 
(test '(100 20 10 2) 'closure-test4 ((cadddr f80-10) 2))

;; A bug reported by Edward Mandac <ed@texar.com>.
(test "Done" 'do-future (do   ((test 'empty))
			  (#t "Done")
			(future (begin(set! test 'goodbye)))))

(define p1 (cons 9 45))
(define-alias p2 p1)
(define-alias p2car (car p2))
(set! p2car 40)
(test '(40 . 45) 'test-alias-1 p1)
(define p1-cdr-loc (location (cdr p1)))
(set! (p1-cdr-loc) 50)
(set! (car p2) 49)
(test '(49 . 50) 'test-alias-2 p2)
(test '(49 . 50) 'test-alias-3 ((location p1)))

(define (test-alias-4 x y)
  (define-alias xcar (car x))
  (define-alias yy y)
  (set! yy (+ yy xcar))
  (set! xcar yy)
  (list yy xcar x y))
(test '(59 59 (59 . 50) 59) test-alias-4 p1 10)

(define (test-alias-5 x)
  (define y (list x))
  (define-alias z y)
  (list x y z))
(test '(8 (8) (8)) test-alias-5 8)

(define test-nesting-1
  (lambda ()
    ((lambda (bar)
       (letrec
	   ((foo 
	     (lambda (bar1) (foo bar))))
	 33))
   100)))
(test 33 test-nesting-1)

(define (test-nesting-2)
  ((lambda (bar1)
     (lambda ()
       (lambda ()
         bar1)))
   #t)
  (let ((bar2 34))
    (lambda () (lambda () bar2))))
(test 34 ((test-nesting-2)))

(define (test-nesting-3 k l m n o)
  (define (foo a b c d e f)
    (list a b c d e f k l m n o))
  (foo foo (+ k k) (+ k l) (+ k m) (+ k n) (+ k o)))
(test '(20 21 22 23 24 10 11 12 13 14) 'test-nesting-3
      (cdr (test-nesting-3 10 11 12 13 14)))

;;; Testcase from  "Walter C. Pelissero" <wcp@lpds.sublink.org>:
(test #t procedure?
      (let* ((is-equal eqv?)
             (false
              (lambda ()
                (is-equal 'bar 'foo)))
             (foo (lambda () 'foo)))
        (lambda ()
          (foo))))

(test #t pair?
      (let* ((is-equal eqv?)
             (false
              (lambda ()
                (is-equal 'bar 'foo)))
             (foo (lambda () (false))))
        (list
         false
         (lambda () (foo)))))

(test #t pair?
      (let* ((is-equal eqv?)
             (false
              (lambda ()
                (is-equal 'bar 'foo)))
             (foo (lambda () (false))))
        (list
         false
         (lambda ()
           (define (bar) (foo))
           (list bar (bar))))))

(test #t not
      (let* ((foo (lambda ()
                    'foo))
             (bar (lambda ()
                    (let loop ((arg 'bar))
                      (foo)
                      (not (loop (foo)))))))
        #f))

(define (test-duplicate-names)
  (let ((bar #t)) (lambda () (lambda () bar)))
  (let ((bar #t)) (lambda () (lambda () bar)))
  (let ((bar #t)) (lambda () (lambda () bar)))
  97)
(test 97 test-duplicate-names)

(test #f 'mutual-recursion-1
      (letrec ((a (lambda () (b)))
               (b (lambda () (a))))
        #f))
(test #f 'mutual-recursion-2
      (letrec ((a (lambda () 10))
               (b (lambda () (a)))
               (c (lambda () (e) (b)))
               (d (lambda () (c)))
               (e (lambda () (d))))
        #f))

;; Used to cause a verification error.
(define (sql-rsmd-all op rsmd . iter)
  (if (null? iter)
      (sql-rsmd-all op rsmd (sql-rsmd-columncount rsmd) '())
      (if (zero? (car iter))
        (cadr iter)
	  (sql-rsmd-all op rsmd (- (car iter) 1)
			      (cons (op rsmd (car iter))
					      (cadr iter))))))
(define (test-location-local x)
  (let* ((xl (location x))  ;; test location of formal parameter x
	 (z (xl))
	 (zl (location z))) ;; test location of local variable z
    (set! (xl) (+ (zl) 100))
    x))
(test 110 test-location-local 10)

(test 15 'tail-call (let loop ((a 1) (b 2) (c 3) (d 4) (e 5) (f 6))
  (if (> a 10) b (loop b c d e f (+ a b c)))))

;;; read-line should handle CR, LF and CRLF equally
(section "read-line")

(define (test-read-split port)
  (call-with-values (lambda () (read-line port 'split))
    (lambda x (car x))))

(define (test-read-line proc)
  (call-with-input-string
   "line\rline\nline\r\nline"
   (lambda (strport)
     (list (proc strport) (proc strport) (proc strport) (proc strport)))))

(test '("line" "line" "line" "line")
      test-read-line
      read-line)

(test '("line" "line" "line" "line")
      test-read-line
      test-read-split)

(define plus10 (make-procedure foo: 33 name: 'Plus10
                            method: (lambda (x y)
				      (+ x (if (number? y) y 0) 10))
                            method: (lambda () 10)))
(test 50 plus10 30 10)
(test 10 plus10)
(test 12 plus10 2 #!null)
;;(test 10 'plus10-error
;;      (try-catch (plus10 3) (ex <java.lang.Exception> "error")))
(test 33 procedure-property plus10 'foo)
(set-procedure-property! plus10 'foo 44)
(test 44 procedure-property plus10 'foo)
(test "#<procedure Plus10>" 'plus10-name1 (format "~s" plus10))
(set-procedure-property! plus10 'name 'PlusTen)
(test "#<procedure PlusTen>" 'plus10-name2 (format "~s" plus10))

(define-procedure Plus
  (lambda ((x :: <number>) (y :: <number>)) (+ x y))
  (lambda ((x :: <string>) (y :: <string>)) (string-append x y)))
(test 12 Plus 5 7)
(test "57" Plus "5" "7")

(define (return-null) #!null)
(test #!null return-null)

;;; Based on bug report 2002-3-1 from Petter &Ouml;sterlund <petos@fyrplus.se>
(define (fie-1) (fie-2) (fie-3))
(define (fie-4) (fie-3) (fie-3)) 
(test #t 'names (and (procedure? fie-1) (procedure? fie-4)))

;; Test from Jim White <jim@pagesmiths.com> - fails if --full-tailscalls.
(define (systime) (invoke-static <java.lang.System> 'currentTimeMillis))
(define systime-1 (systime))
(define systime-2 (systime))
(test #t >= systime-2 systime-1)

;; Bug reported by Wen-Chun Ni <wcn@tbcommerce.com>.
(define (fl-f y) (+ 10 y))
(fluid-let ((fl-x 2)) (fl-f 1))

(test "10a" 'to-hex-1 (java.lang.Integer:toHexString 266))
(define (to-hex (x :: <int>)) (java.lang.Integer:toHexString x))
(test "10b" to-hex 267)
(define-namespace Long "class:java.lang.Long")
(test "10d" 'to-hex-1 (Long:toHexString 269))
(define (long-to-hex (x :: <long>)) (Long:toHexString x))
(test "10e" long-to-hex 270)
(test "123" Long:toString (Long:new '00123))
(define (to-int-string x :: <long>) (java.lang.Object:toString (Long:new x)))
(test "124" to-int-string '00124)

;;; Based on bug report 2002-12-3 from Petter &Ouml;sterlund <petos@fyrplus.se>
(define (fie-6)
 6
  (define (runn)
    foo)
  (define (foo)
   'done)
  (apply runn '())) 
(test 'done 'call-fie-6 ((fie-6)))

;; Test instance?
(test #t instance? 1 <number>)
(test #t instance? "x" <string>)
(test #f instance? "x" <number>)
(test #f instance? #!null <string>)
;; Tests based on Savannah bug #31353 "boolean type"
(test #t instance? #f boolean)
(test #f instance? 123 boolean)
(test #t 'test-instance (instance? #f boolean))
(test #f 'test-instance (instance? 123 boolean))
(define (is-prim-boolean x)
   (instance? x boolean))
(test #f is-prim-boolean 123)
(test #t is-prim-boolean #t)

;; Based on a bug reported 05-26 Sven.Hartrumpf@FernUni-Hagen.de
(define (list-cond compare a b)
  (cons (compare a (list b)) b))
(define (make-mf forms results)
  (let ((r
  (map
   (lambda (result)
     (map
      (lambda (form)
	(list-cond
	 (lambda (a b)
	   (string<? (cadr a) (caar b)))
	 forms
	 (list form)))
      forms))
   results)))
  (call-with-output-string
   (lambda (output-stream)
     (for-each
      (lambda (form)
	(format output-stream "[f:~a]" form))
      r)))))
(test "[f:((#f a1) (#f a2))][f:((#f a1) (#f a2))]"
      make-mf '("a1" "a2") '("b1" "b2"))

(require 'printf)
(define (test-printf format value)
  (call-with-output-string
   (lambda (out)
     (fprintf out format value))))
(test "[ 23]" test-printf "[%3d]" 23)
(test "[3.50 ]" test-printf "[%-5.2f]" 3.5)

(define fluid-stack '())
(define fluid-let-test-level 'main)
(define (push-fluid-let-test-level!)
  (set! fluid-stack (cons fluid-let-test-level fluid-stack)))
(define (test-fluid-let-levels)
  (push-fluid-let-test-level!)
  (force
   (future
    (fluid-let ((fluid-let-test-level 'thread))
      (push-fluid-let-test-level!)
      (force (future (push-fluid-let-test-level!))))))
  fluid-stack)

(test '(thread thread main) test-fluid-let-levels)

(define (not-a) ((lambda (x) (not x)) 'a))
(test #f not-a)

(test "Test." 'from-psyntax
      ((lambda ()
	 (letrec ((topfun
		   (lambda (marks)
		     ((lambda ()
			((lambda ()
			   (lambda () marks)))))))
		  (chifun
		   (lambda () (list topfun))))
	   "Test."))))

(require 'list-lib)

(test '(1 3) 'filter!-test (filter! odd? (iota 5)))

;; Test fluid-let in the presence of threads
(define-variable *X* #f)

(define (get-*X*)
  *X*)

;; Should return '(1 2)
(define (fluid-let-and-threads)
  (let* ((t1 (future
              (begin
                (fluid-let ((*X* 1))
                  (sleep 0.5)
                  (get-*X*)))))
         (t2 (future
              (begin
                (sleep 0.25)
                (fluid-let ((*X* 2))
                  (sleep 0.5)
                  (get-*X*))))))
    (list (force t1) (force t2))))

(test '(1 2) fluid-let-and-threads)

(define param1 (make-parameter 10 number->string))
(test "10" 'param-test1 (param1))
(define-alias param1v (param1))
(set! (param1) 11)
(test "11" 'param-test2 param1v)
(param1 12)
(test "12" 'param-test3 (param1))
(set! param1v 13)
(test "13" 'param-test4 (param1))
(test '("15" "15" "16" "16" "13" "13") 'param-test5
      (let ((r0
	     (fluid-let ((param1v (+ (string->number param1v) 2)))
	       (let ((r1 (list (param1) param1v)))
		 (set! param1v 16)
		 (append r1 (list (param1) param1v))))))
	(append r0 (list (param1) param1v))))
(param1 20)
(test '("22" "22" "17" "17" "20" "20") 'param-test5
      (let ((r0
	     (parameterize ((param1 (+ (string->number (param1)) 2)))
	       (let ((r1 (list (param1) param1v)))
		 (set! param1v 17)
		 (append r1 (list (param1) param1v))))))
	(append r0 (list (param1) param1v))))

(begin
  (define var1 1)
  (test 2 'test-fluid-future-1a
        (force
         (fluid-let ((var1 2))
           (future (begin  (sleep 0.1s) var1)))))
  (test 1 'test-fluid-future-1b var1))

(define-variable var2 1)
(test 2 'test-fluid-future-2a
      (force
       (fluid-let ((var2 2))
         (future (begin  (sleep 0.1s) var2)))))
(test 1 'test-fluid-future-2b var2)

;; Bug reported 2005-05-08 by dominique.boucher@nuecho.com.
(require <moduleFT>)
(define (test-neg-abs)
  (let ((x (neg-abs 4)))
    (format #f "x = ~S." x)))
(test "x = -4." test-neg-abs)

(test '((prefix-test 11)
	(prefix-test:var2 12)
	(prefix-test:var2:var3 13)
	(prefix-test:filler:var4 14))
      'prefix-test
      prefix-test-list)
(test '(12) 'prefix-test:var2 prefix-test:var2)
(test '(13) 'prefix-test:var2:var3 prefix-test:var2:var3)
(test '(14) 'prefix-test:filler:var4 prefix-test:filler:var4)

;; Common Lisp hyperspec
(test "[#24rn]" 'print-base-1 ;; Common Lisp returns upper-case #24rN
      (fluid-let ((*print-base* 24) (*print-radix* #t))
	(format #f "[~s]" 23)))
(test '("101000" "1111" "220" "130" "104" "55" "50" "44" "40" "37" "34"
	"31" "2c" "2a" "28" "26" "24" "22" "20" "1j" "1i" "1h" "1g" "1f"
	"1e" "1d" "1c" "1b" "1a" "19" "18" "17" "16" "15" "14") 'print-base-2
	;print the decimal number 40 in each base from 2 to 36
      (let loop ((i 36) (r '()))
	(if (= i 1) r
	    (loop (- i 1)
		  (cons (fluid-let ((*print-base* i)) (format #f "~s" 40))
			r)))))
(test '("#b1010 #b1/1010" "#3r101 #3r1/101" "#o12 #o1/12" "10. #10r1/10" "#xa #x1/a") 'print-base-3
      ;;print the integer 10 and the ratio 1/10 in bases 2, 3, 8, 10, 16 
      (map (lambda (pb)
	     (fluid-let ((*print-radix* #t) (*print-base* pb))
	       (format #f "~S ~S" 10 1/10)))
	   '(2 3 8 10 16)))

;; Savannah bug #14697 Error using :: <int>
;; Submitted by:  	Gerardo Horvilleur <mago>
(define bug14697-result "")
(let ((GS.261 :: <int> 10)
      (GS.262 :: <int> 1))
  (do ((i :: <int> 1 (+ i GS.262)))
      ((> i GS.261))
    (set! bug14697-result (string-append bug14697-result " "
					 (number->string i)))))
(test " 1 2 3 4 5 6 7 8 9 10" 'bug14697 bug14697-result)

(require 'xml)
(test "<code xmlns=\"http://www.w3.org/1999/xhtml\">Foo</code>" 'html-contructor-1
      (as-xml (html:code "Foo")))
(test "<a xmlns=\"http://www.w3.org/1999/xhtml\" href=\"foo.html\">Foo</a>" 'html-contructor-2
      (as-xml (html:a href:"foo.html" "Foo")))
(define-xml-namespace h "HTML")
(test "<h:code xmlns:h=\"HTML\">Foo</h:code>" 'html-contructor-3
      (as-xml (h:code "Foo")))
(test "<b xmlns=\"http://www.w3.org/1999/xhtml\"><code>Foo</code></b>" 'html-contructor-4
      (as-xml (html:b (html:code "Foo"))))

(test "<code xmlns=\"http://www.w3.org/1999/xhtml\">Foo</code>" 'html-contructor-1lit
      (as-xml #<html:code>Foo</html:code>))
(test "<a xmlns=\"http://www.w3.org/1999/xhtml\" href=\"foo.html\">Foo</a>" 'html-contructor-2lit
      (as-xml #<html:a ['href]="&["foo"].&(string-append "ht" "ml")">Foo</>))
;; old syntax
(test "<a xmlns=\"http://www.w3.org/1999/xhtml\" href=\"foo.html\">Foo</a>" 'html-contructor-2lit
      (as-xml #<html:a ['href]="&["foo"].&(string-append "ht" "ml")">Foo</>))
(define-xml-namespace h "HTML")
(test "<h:code xmlns:h=\"HTML\">Foo</h:code>" 'html-contructor-3lit
      (as-xml #<h:code>Foo</>))
(test "<b xmlns=\"http://www.w3.org/1999/xhtml\"><code>Foo</code></b>" 'html-contructor-4lit
      (as-xml #<html:b><html:code>Foo</></>))
(test "<b xmlns=\"http://www.w3.org/1999/xhtml\"><code>FooBar</code></b>" 'html-contructor-4enc
      (let ((body1 "Foo")
	    (body2 "Bar")
	    (code 'html:code))
	(as-xml #<[(quote html:b)]><[code]>&[body1]&(car (list body2))</></>)))

(test "<list><b xmlns=\"http://www.w3.org/1999/xhtml\">bold 1</b> <b xmlns=\"http://www.w3.org/1999/xhtml\">bold2</b></list>" 'html-contructor-5 (as-xml (map html:b '("bold 1" "bold2"))))

;; Test for Savannah bug #18909 "Recursive call to function in closure causes
;; NullPointerException".  Chris Wegrzyn <chris.wegrzyn@gmail.com>
(define (savannah-18909-outerproc foo)
  (define (innerproc)
    (if foo
	(lambda () (innerproc))
	'()))
  (innerproc))
(define savannah-18909-destroy ((savannah-18909-outerproc #t))) 
(test savannah-18909-destroy 'savannah-18909 (savannah-18909-destroy))

;; Bug reported by Yaroslav Kavenchuk <kavenchuk@jenty.by> 2008-02-26:
(define primes (<integer[]> 2 3 5 7 11 13))
(test 11 'primes-integer-indexing (primes 4))

(define sum 0)
(define (test-exit-with-finally-1 x)
  (call-with-current-continuation
   (lambda (exit)
     (try-finally
      (if (< x 0)
	  (exit (list x))
	  (* 2 x))
      (set! sum (+ sum 1))))))
(test '(8 10 (-9) (-1) 24) 'test-exit-with-finally-1
      (map test-exit-with-finally-1 '(4 5 -9 -1 12)))

(set! sum 0)
(define list-inner '())
(define (test-exit-with-finally-2 x)
  (call-with-current-continuation
   (lambda (exit1)
     (try-finally
      (call-with-current-continuation
       (lambda (exit2)
	 (try-finally
	  (begin
	    (if (< x 0)
		(exit2 (list 2 x)))
	    (if (odd? x)
		(exit1 (list 1 x)))
	    (set! list-inner (cons x list-inner))
	    (list 0 x))
	  (set! sum (+ sum 1)) #| Inner finally |#)))
      (set! sum (+ sum 10)) #| Outer finally |#))))
(test '((0 4) (1 5) (2 -9) (0 14) (2 -1) (0 12) (1 7))
	'test-exit-with-finally-2
      (map test-exit-with-finally-2 '(4 5 -9 14 -1 12 7)))
(test "Sum: 77 Inner: (12 14 4)"
      'test-exit-with-finally-2-results
      (format #f "Sum: ~s Inner: ~s" sum list-inner))

;; R6RS and SRFI-62 S-expression comments
(test 5 'srfi-62-test-1 (+ 1 #;(* 2 3) 4))
(test '(x z) 'srfi-62-test-2 (list 'x #;'y 'z))
(test 12 'srfi-62-test-3 (* 3 4 #;(+ 1 2)))
(test 16 'srfi-62-test-4 (#;sqrt abs -16))
(test '(a d) 'srfi-62-test-5 (list 'a #; #;'b 'c 'd))
(test '(a e) 'srfi-62-test-6 (list 'a #;(list 'b #;c 'd) 'e))
(test '(a . c) 'srfi-62-test-7 '(a . #;b c))
(test '(a . b) 'srfi-62-test-8 '(a . b #;c))

;; Savannah bug #26940 "Compiler stuck in endless loop"
;; Reported by Helmut Eller
(define (mutual-tailcals x)
  ;; Note that the order of the functions is reversed relative to the
  ;; Savannah bug report, because I recently fixed the implementation of
  ;; the letrec macro, which used to create declarations in reverse order.
  (letrec ((f0 (lambda () (if (= x 0) 1 (f1))))
           (f1 (lambda () (if (= x 0) (f0) (f2))))
	   (f2 (lambda () (if (= x 0) (f1) 0))))
    (f2)))
(test 0 mutual-tailcals 4)

;; Savannah bug #24249 "Local define miscompiled"
(let ()
  ;; Added 'list' to suppress tail-call-optimization.
  (define (baz) (list (bar)))
  (define (bar)
    (let ((k (lambda () #f)))
      (cond ((not (procedure? k))
	     (error 'bad-k k)))
      k))
  (define (foo) (bar))
  (test "#<procedure k>" 'test-savannah-24249 ((foo):toString)))

(require <InliningTest>)
(test 16 inline-two-calls 5)
(test 7 inline-two-calls -5)
(test #f check-even 200001)
(test #t check-even 18)
(test #f check-even-unspec-return 23)

;; Savannah bug #27011: ArrayIndexOutOfBoundsException after 20 local variables
(define (big-let) ; no argument!
  (define x0  "a")
  (define x1  "a")
  (define x2  "a")
  (define x3  "a")
  (define x4  "a")
  (define x5  "a")
  (define x6  "a")
  (define x7  "a")
  (define x8  "a")
  (define x9  "a")
  (define x10 "a")
  (define x11 "a")
  (define x12 "a")
  (define x13 "a")
  (define x14 "a")
  (define x15 "a")
  (define x16 "a")
  (define x17 "a")
  (define x18 "a")
  (define x19 "a")
  (define x20 "a")
  (set! x0  x1 )
  (set! x1  x2 )
  (set! x2  x3 ) 
  (set! x3  x4 )
  (set! x4  x5 )
  (set! x5  x6 )
  (set! x6  x7 )
  (set! x7  x8 )
  (set! x8  x9 )
  (set! x9  x10)
  (set! x10 x11)
  (set! x11 x12)
  (set! x12 x13)
  (set! x13 x14)
  (set! x14 x15)
  (set! x15 x16)
  (set! x16 x17)
  (set! x17 x18)
  (set! x18 x19)
  (set! x19 x20)
  (set! x20 x0 ) 
  x0)
(test "a" big-let)

;; Savannah bug #27019 "setLength method of StringBuilder not found"
(define sb (java.lang.StringBuilder "abcdef"))
(define (set-length (builder :: java.lang.StringBuilder) (len :: int))
  (with-compile-options warn-invoke-unknown-method: #t
			warn-as-error: #t
			(builder:setLength len)))
(set-length sb 4)
(test "abcd" 'test-savannah-27019 (sb:toString))

;; Savannah bug #27188 "Sequence printing"
(define sublist-27188 (invoke #(10 11 12 13 14 15) 'subList 1 3))
(test "(#sequence 11 12)" 'test-savannah-27188 (format #f "~s" sublist-27188))

(require "test-cycle12.scm")
(test '(8 12) c1x-c2x)

;; Savannah bug #27257 "non-int dim. spec. in emitNewArray"
(define (alloc-array count val)
  (object[] length: (+ 1 count) 1: val 2: (+ 1 val)))
(test "[#!null 10 11 #!null #!null]" 'test-savannah-27257
      (format #f "~s" (alloc-array 4 10)))

(define shared-1 '(#2=(3 4) 9 #2# #2#))
(test '((3 4) 9 (3 4) (3 4)) 'shared-1 shared-1)

(test 25 'multiple-do-with-type-specs
      (let ((ll '(1 3 5 7 9)))
	(do ((x :: list ll (cdr x))
	     (sum :: int 0 (+ sum (car x))))
	    ((null? x) sum))))

;; Savannah bug #28957: exception in inliner, from Helmut Eller
(test "Type java.lang.CharSequence[]" 'test-savannah-28957
      (((string[]):getClass):toString))

;; Savannah bug #28926: EOL conversion in READ
(test "(#\\X #\\return #\\Y #\\return #\\newline #\\Z)" 'test-savannah-28926
      (format "~w" (let ((in (string #\" #\X #\return #\Y #\return #\newline #\Z #\")))
		     (string->list (call-with-input-string in read)))))

;; Savannah bug #31250: try/catch & endless loop
(define (test-savannah-31250 f)
  (try-catch
   (let loop ()
     (f)
     (loop))
   (e java.lang.Exception 
      (e:printStackTrace))))

;; Savannah bug #32656: ArrayIndexOutOfBoundsException in mergeLocalType
(test 2 'savannah-32656
      (letrec ((f (lambda (x)
                    (case x
                      ((0) (f x))
                      ((1) (g x))
                      ((2) (h x)))))
               (g (lambda (x)
                    (case x
                      ((0) (f x))
                      ((1) (g x))
                      ((2) (h x)))))
               (h (lambda (x)
                    (case x
                      ((0) (f x))
                      ((1) (g x))
                      ((2) ;(h x)
                       x)))))
        (f 2)))

;; Savannah bug #32657: Verification error with JDK7
(begin
  (define (foo-savannah-32657) ()
    (let ((x (bar-savannah-32657))
          (fail (lambda () (error "fail"))))
      (if (instance? x <pair>)
          (let ((y :: <pair> x))
            (let ((z (y:getCar)))
              (if (eq? (y:getCdr) '())
                  z
                  (fail))))
          (fail))))
  (define (bar-savannah-32657) ::<list>
    (list 1))
  (test 1 'savannah-32657 (foo-savannah-32657)))

;; Testcase simplified from slime/config/swank-kawa.scm
(define-syntax mif
  (syntax-rules ()
    ((mif ((p . ps) value) then)
     (let ((fail (lambda () (error "mlet failed")))
           (tmp value))
       (if (instance? tmp <pair>)
           (let* ((tmp :: <pair> tmp))
             then)
           (fail))))))
(define (dispatch-events)
  (let ((tmp0 '(a b c)))
    (mif ((c . event) tmp0)
            1234)))
(test 1234 dispatch-events)

;; Savavvah bug #36592 "nested map causes compiler inliner NPE"
(test '(1 2 3) 'savannah-36592
      (map (lambda (x) x) (map (lambda (x) x) '(1 2 3))))

(define falseBool1 (java.lang.Boolean #f))
(define falseBool2 (java.lang.Boolean #f))
(test #f 'eq1-falseBool (eq? falseBool1 falseBool2))
(test #f 'eq2-falseBool (apply eq? falseBool1 falseBool2 '()))
(test #t 'eqv1-falseBool (eqv? falseBool1 falseBool2))
(test #t 'eqv2-falseBool (apply eqv? falseBool1 falseBool2 '()))
(test #t 'eqv3-falseBool (eqv? falseBool1 #f))
(test #t 'equal1-falseBool (equal? falseBool1 falseBool2))
(test #t 'equal2-falseBool (apply equal? falseBool1 falseBool2 '()))

;; # is a terminating macro character in Scheme.
(test '(a b) 'adjacent-sharp-comment '(a#|com|#b))

;; Savannah bug report #39944 "Possible bug with omitted keyword arguments"
(define (f-39944 #!key (y -1) (z -2)) z)
(test 42 'savannah-39944 (f-39944 z: 42))
