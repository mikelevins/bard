(test-init "macros" 127)

(test 'ok 'letxx (let ((xx #f)) (cond (#t xx 'ok))))

(test 'ok 'let=> (let ((=> #f)) (cond (#t => 'ok))))

(begin-for-syntax
 (load (string-append src-prefix "mac1.scm")))
(test '(1 2) 'something (something 1 2))

(test '(2 3) 'something (something 2 3))

;;; From Common Lisp the Language 2nd ed page 198
(defmacro arithmetic-if (test neg-form zero-form pos-form)
  (let ((var (gentemp)))
    `(let ((,var ,test))
       (cond ((< , var 0) ,neg-form)
             ((= ,var 0) ,zero-form)
             (#t ,pos-form)))))

(test "POS" 'arithmetic-if-pos (arithmetic-if 234 "NEG" "ZERO" "POS"))
(test "NEG" 'arithmetic-if-pos (arithmetic-if -234 "NEG" "ZERO" "POS"))

;;; Posted to comp.lang.scheme by mooreb@lark.cc.ukans.edu (Brian M. Moore)
(test '(x) 'lambda*3
      ((lambda lambda lambda) 'x))
(test '(1 2 3) 'lambda-begin
      ((lambda (begin) (begin 1 2 3)) (lambda lambda lambda)))

;;; From R5RS:
(test 'now 'let-syntax-1
      (let-syntax
          ((when (syntax-rules ()
                               ((when test stmt1 stmt2 ...)
                                (if test
                                    (begin stmt1 stmt2 ...))))))
        (let ((if #t))
          (when if (set! if 'now))
          if)))

;;; From R5RS:
(test 'outer 'let-syntax-2
      (let ((x 'outer))
        (let-syntax ((m (syntax-rules () ((m) x))))
          (let ((x 'inner))
            (m)))))                ;       =>  outer


;;; Based on an example Listed as an "error" in R5RS.
;;; (We don't actually complain about the erroneous version.)
(test 6 'let-syntax-3
      (let-syntax
          ((foo (syntax-rules ()
                              ((foo (proc args ...) body ...)
                               (define proc
                                 (lambda (args ...)
                                   body ...))))))
        (let ((x 3))
          (foo (plus x y) (+ x y))
          (let () ;; Added this extra let to make it legit.
            (define foo x)
            (plus foo x)))))

;;; From R5RS:
(test 7 'letrec-syntax-1
      (letrec-syntax
       ((my-or (syntax-rules ()
                             ((my-or) #f)
                             ((my-or e) e)
                             ((my-or e1 e2 ...)
                              (let ((temp e1))
                                (if temp
                                    temp
                                    (my-or e2 ...)))))))
       (let ((x #f)
             (y 7)
             (temp 8)
             (let odd?)
             (if even?))
         (my-or x
                (let temp)
                (if y)
                y))))

(define (internal-define-syntax)
  (let ()
    (define-syntax ten (syntax-rules () ((ten) 10)))
    (define x (ten))
    x))
(test 10 internal-define-syntax)

;; Based on bug report from Stephen L. Peters <portnoy@portnoy.org>:
(define-syntax test-ds1 (syntax-rules () ((test-ds1 x) (list 'x))))
(test '((t1)) 'test-ds1 (test-ds1 (t1)))
(test '((t2)) 'test-ds2
      (begin
	(define-syntax test-ds2 (syntax-rules () ((test-ds2 x) (list 'x))))
	(test-ds2 (t2))))

(define x 1)
(define y 2)
(define z 3)
(define-syntax test-ds3
  (syntax-rules () ((test-ds3 x y) (let ((y x) (x z) (z y)) (list x y z)))))
(test '(3 2 3) 'test-ds3 (test-ds3 y z))

(test '() 'cut-1 ((cut list)))
(test '() 'cut-2  ((cut list <...>)))
(test '(1) 'cut-3 ((cut list 1)))
(test '(1) 'cut-4 ((cut list <>) 1))
(test '(1) 'cut-5 ((cut list <...>) 1))
(test '(1 2) 'cut-6 ((cut list 1 2)))
(test '(1 2) 'cut-7 ((cut list 1 <>) 2))
(test '(1 2) 'cut-8 ((cut list 1 <...>) 2))
(test '(1 2 3 4) 'cut-9 ((cut list 1 <...>) 2 3 4))
(test '(1 2 3 4) 'cut-10 ((cut list 1 <> 3 <>) 2 4))
(test '(1 2 3 4 5 6) 'cut-11 ((cut list 1 <> 3 <...>) 2 4 5 6))
(test '(ok) 'cut-12 (let* ((x 'wrong) (y (cut list x))) (set! x 'ok) (y)))
(test 2 'cut-13
      (let ((a 0))
	(map (cut + (begin (set! a (+ a 1)) a) <>)
	     '(1 2))
	a))

;; cutes
(test '() 'cute-1 ((cute list)))
(test '() 'cute-2 ((cute list <...>)))
(test '(1) 'cute-3 ((cute list 1)))
(test '(1) 'cute-4 ((cute list <>) 1))
(test '(1) 'cute-5 ((cute list <...>) 1))
(test '(1 2) 'cute-6 ((cute list 1 2)))
(test '(1 2) 'cute-7 ((cute list 1 <>) 2))
(test '(1 2) 'cute-8 ((cute list 1 <...>) 2))
(test '(1 2 3 4) 'cute-9 ((cute list 1 <...>) 2 3 4))
(test '(1 2 3 4) 'cute-10 ((cute list 1 <> 3 <>) 2 4))
(test '(1 2 3 4 5 6) 'cute-11 ((cute list 1 <> 3 <...>) 2 4 5 6))
(test 1 'cute-12
      (let ((a 0))
	(map (cute + (begin (set! a (+ a 1)) a) <>)
	     '(1 2))
	a))

(define-syntax test-set
 (syntax-rules ()
   ((test-set)
    (let ((s 1))
      (set! s (+ s 1))
      s))))
(test 2 'test-set (test-set))

(define-syntax test-colon
 (syntax-rules ()
   ((test-colon x)
    (let loop ((size :: <int> 10))
      (+ size x)))))
(test 14 'test-colon (test-colon 4))

;; Bug reported by 2003-05-22 by Bruce R. Lewis <brlewis@ALUM.MIT.EDU>.
(define-syntax one
  (syntax-rules ()
		((one var)
		 (begin
		   (define extra 1)
		   (define var extra)))))
(one xx1)
(test 1 'one xx1)

;; Bug reported by 2003-05-22 by Bruce R. Lewis <brlewis@ALUM.MIT.EDU>.
(define-syntax lit1
  (syntax-rules (literal)
		((lit1 literal)
		 "worked")
		((lit1 a)
		 (lit1 literal))))
(test "worked" 'lit1 (lit1 20))

;; Based on bug reported 2003-05-19 by Sven.Hartrumpf@FernUni-Hagen.de
(test "no-bogus-feature" 'cond-expand-not-1
      (cond-expand ((not bogus-feature)
		    "no-bogus-feature")
		   (else "has-bogus-feature")))
(test "has-srfi-4" 'cond-expand-not-2
      (cond-expand ((not srfi-4)
		    "no-srfi-4")
		   (else "has-srfi-4")))

;; Based on bug reported 2003-06-01 by Sven.Hartrumpf@FernUni-Hagen.de
(cond-expand (kawa
	      (define found-kawa-feature "yes"))
	     (else
	      (define found-kawa-feature "no")))
(test "yes" 'found-kawa-feature found-kawa-feature)

;; Based on bug reported 2003-06-02 by Sven.Hartrumpf@FernUni-Hagen.de
(cond-expand 
 ((not no-such-srfi)
  (define third caddr))
 (else))
(test 'z third '(x y z))

(test 1 'test-class-exists-1
      (cond-expand (class-exists:java.lang.StringBuilder 1) (else 0)))
(test 0 'test-class-exists-2
      (cond-expand (class-exists:java.lang.StringMunger 1) (else 0)))

(define-syntax or-with-keyword-test
  (syntax-rules (default-value:)
    ((or-with-keyword-test val default-value: default)
     (if val
         val
         default))
    ((or-with-keyword-test val)
     (or-with-keyword-test default-value: #f))))

(test 'ok 'or-with-keyword-test (or-with-keyword-test #f default-value: 'ok))

;; Al Petrofsky posting to comp.lang.scheme 2002-03-03:
(test "(1 2 3 a)" 'letrec-test
      (format #f "~S"
	      (let ((a 1))
		(letrec-syntax
		 ((foo (syntax-rules ()
				     ((_ b)
				      (bar a b))))
		  (bar (syntax-rules ()
				     ((_ c d)
				      (cons c (let ((c 3))
					(list d c 'c)))))))
		 (let ((a 2))
		   (foo a))))))

;; A posting by Taylor Campell to comp.lang.scheme 2004/10/9:
(test #(1 2 unquote (list 3 4)) 'unquote-vector `#(1 2 unquote (list 3 4)))

;; Example in Dybvig's "The Scheme Programming Language" 3rd ed chapter 8:
(test #t 'dybvig-SchemePL3-8Syntax-ex1
      (let ()
	(define even? (lambda (x) (or (= x 0) (odd? (- x 1)))))
	(define-syntax odd? (syntax-rules () ((_ x) (not (even? x)))))
	(even? 10)))
;; Example in Dybvig's "The Scheme Programming Language" 3rd ed chapter 8:
(test 0 'dybvig-SchemePL3-8Syntax-ex2
      (let ()
	(define-syntax bind-to-zero
	  (syntax-rules () ((_ id) (define id 0))))
	(bind-to-zero x)
	x))

(test '(1 2) 'dybvig-SchemePL3-8Syntax-ex3
      (let ((f (lambda (x) (+ x 1))))
	(let-syntax ((f (syntax-rules () ((_ x) x)))
		     (g (syntax-rules () ((_ x) (f x)))))
	  (list (f 1) (g 1)))))

(test '(1 1) 'dybvig-SchemePL3-8Syntax-ex4
      (with-compile-options
       warn-unused: #f
       (let ((f (lambda (x) (+ x 1))))
         (letrec-syntax ((f (syntax-rules () ((_ x) x)))
                         (g (syntax-rules () ((_ x) (f x)))))
           (list (f 1) (g 1))))))

;; Savannah bug report #10561 from Chris Dean
(define-syntax log-mode
  (syntax-rules ()
    ((log-mode mode)
     (case 'mode
       ((error) "error mode")
       ((warning) "warning mode")
       (else "bad mode")))))
(test "warning mode" 'log-mode (log-mode warning))

;; Savannah bug report #9483
(define-syntax macro-chain
  (syntax-rules ()
    ((macro-chain . z)
     (letrec-syntax
	 ((m1 (syntax-rules () ((m1 x) (id (m2 x)))))
	  (m2 (syntax-rules () ((m2 x) (id (m3 x)))))
	  (m3 (syntax-rules () ((m3 x) (quote x))))
	  (id (syntax-rules () ((id x) x))))
       (m1 z)))))
(test '(1) 'macro-chain (macro-chain 1))

;; From FLT MzScheme Manual section 12.3.5 Macro-Gnerated Top-Level
(define-syntax def-and-use-of-x
  (syntax-rules ()
    ((def-and-use-of-x val)
     ; x below originates from this macro:
     (begin (define x val) x))))
(define x1 1)
(test 2 'mzscheme-lang-12.3.5-1 (def-and-use-of-x 2))
(test 1 'mzscheme-lang-12.3.5-2 x1)

;; From FLT MzScheme Manual section 12.3.5 Macro-Generated Top-Level
(define-syntax def-and-use
  (syntax-rules ()
    ((def-and-use x val)
     ; x below was provided by the macro use:
     (begin (define x val) x))))
(set! x 2)
(test 3 'mzscheme-lang-12.3.5-3 (def-and-use x 3))
(set! fail-expected "mzscheme-lang-12.3.5-4 is 2 but should be 3")
;; Note this works if def-and-use uses set! instead of define.
;; Probably chalk this up to Kawa's top-level define being different.
(test 3 'mzscheme-lang-12.3.5-4 x)

;; Example from Chez Scheme User's Guide by Kent Dybvig:
(define-syntax loop
  (lambda (x)
    (syntax-case x ()
      ((k e ...)
       (with-syntax ((break (datum->syntax #'k 'break)))
                    #'(call-with-current-continuation
                       (lambda (break)
                         (let f () e ... (f)))))))))
(test '(a a a) 'test-loop-macro
      (let ((n 3) (ls '()))
	(loop
	 (if (= n 0) (break ls))
	 (set! ls (cons 'a ls))
	 (set! n (- n 1)))))

;; Based on SRFI-57 reference implementation by Andre van Tonder.
(define-syntax top:if-free=
  (syntax-rules ()
    ((top:if-free= x y kt kf)
     (begin
       (define-syntax if-free=:test
         (syntax-rules (x)
           ((if-free=:test x kt* kf*) kt*)
           ((if-free=:test z kt* kf*) kf*)))
       (if-free=:test y kt kf)))))
(define-syntax free=
  (syntax-rules ()
    ((free= x y)
     (let () (top:if-free= x y #t #f)))))
(test '(#t #f #t #f) 'test-free=
      (list (free= x x) (free= y x) (free= abba abba) (free= y x)))

(define-syntax check-matching
  (syntax-rules ()
    ((check-matching 1 #(a b c)) (list c #(b a)))
    ((check-matching 2 #(a b ... c)) (list c b ... a))
    ((check-matching 3 a b ... c) #(c b ... a))
    ((check-matching 4 a b c ...) (list  a b c ...))
    ((check-matching 5 a b c ... z) (list a b c ... 'last z))
    ((check-matching 6 a b c ... z . r) (list a b c ... 'last z 'cdr r))
    ((check-matching 7 a 1 ... 1 . r) (list a 'cdr r))
    ((check-matching 8 (a ...) ... (r ...))
     (list (+ a ...) ... 'R r ... 'F (+ a ... ...)))
    ((check-matching 9 (a ...) ...) (list #(A a ... ... Z) '(A a ... ... Z)))
    ((check-matching a . b) "no-match")
    ))
(test '(3 #(2 1)) 'check-matching-1 (check-matching 1 #(1 2 3)))
(test "no-match" 'check-matching-2a (check-matching 2 1 2 3 4 5))
(test '(5 2 3 4 1) 'check-matching-2 (check-matching 2 #(1 2 3 4 5)))
(test #(5 2 3 4 1) 'check-matching-3 (check-matching 3 1 2 3 4 5))
(test "no-match" 'check-matching-4a (check-matching 4 1))
(test '(1 2) 'check-matching-4b (check-matching 4 1 2))
(test '(1 2 3) 'check-matching-4c (check-matching 4 1 2 3))
(test '(1 2 3 4) 'check-matching-4d (check-matching 4 1 2 3 4))
(test "no-match" 'check-matching-5a (check-matching 5 1))
(test "no-match" 'check-matching-5b (check-matching 5 1 2))
(test '(1 2 last 3) 'check-matching-5c (check-matching 5 1 2 3))
(test '(1 2 3 last 4) 'check-matching-5d (check-matching 5 1 2 3 4))
(test '(1 2 3 4 last 5) 'check-matching-5e (check-matching 5 1 2 3 4 5))
(test '(1 2 3 last 4 cdr 5) 'check-matching-6a (check-matching 6 1 2 3 4 . 5))
(test '(1 2 3 4 last 5 cdr ()) 'check-matching-6b (check-matching 6 1 2 3 4 5))
(test '(10 cdr ()) 'check-matching-7a (check-matching 7 10 1 1 1))
(test '(10 cdr 100) 'check-matching-7b (check-matching 7 10 1 1 1 . 100))
(test '(10 cdr 100) 'check-matching-7c (check-matching 7 10 1 . 100))
(test "no-match" 'check-matching-7d (check-matching 7 10 . 100))
(test '(3 12 R 8 9 F 15)
      'check-matching-8 (check-matching 8 (1 2) (3 4 5) (8 9)))
(test '(#(A 1 2 3 4 5 Z) (A 1 2 3 4 5 Z))
      'check-matching-9 (check-matching 9 (1 2) (3 4 5)))

;; Savannah bug #13821
(define-macro (test-13821 #!key (args ()))
  (letrec ((double (lambda (x) (* x 2))))
    `(+ ,@(map double args))))
(test 12 'test-13821 (test-13821 args: (1 2 3)))

;; Savannah bug #14097
(define-syntax slot
  (syntax-rules ()
    ((_ obj slotname)
     (field obj (quote slotname)))
    ((_ slotname)
     (field (this) (quote slotname)))))
(define-simple-class <xclass> ()
  (x init: 0)
  ((incx) <void>
   (set! (slot x) (+ 1 (slot x))))
  ((incx2) <void>
   (set! (slot (this) x) (+ 1 (slot (this) x)))))
(define xinstance (make <xclass>))
(with-compile-options warn-invoke-unknown-method: #f
		      (invoke xinstance 'incx))
(test 1 'xclass (slot xinstance x))

(define x (list "X1" "X2"))
(define y (list "Y1" "Y2"))
;; See Bawden: Quasiquotation in Lisp (1999), Appendix B.
(test '(a ("X1" "X2") ("Y1" "Y2") b) 'unquote-1
      (quasiquote (a (unquote  x y) b)))
(test '(a "X1" "X2" "Y1" "Y2" b) 'unquote-2
      (quasiquote (a (unquote-splicing  x y) b)))

;; Savannah bug #39501 "invalid use of unquote-splicing"
(define-macro (a-39501)
  `(define-macro (b-39501 . x) `(+ 1 ,@x)))
(a-39501)
(test 7 'savannah-39501 (b-39501 1 2 3))

(begin ;; Test that we can define and use a syntax-case macro in same module.
  (define-syntax local-defmac-or
    (lambda (x)
      (syntax-case x ()
		   ((_) (syntax #f))
		   ((_ e) (syntax e))
		   ((_ e1 e2 e3 ...)
		    (syntax
		     (let ((t e1)) (if t t (local-defmac-or e2 e3 ...))))))))
  (test 4 'local-defmac-or (local-defmac-or #f 4 5)))

(test '(2 1) 'srfi-72-example-1
      (let-syntax ((main (lambda (form)
			   (define (make-swap x y)
			     #`(let ((t #,x))
                                 (set! #,x #,y)
                                 (set! #,y t)))
			   #`(let ((s 1)
                                   (t 2))
                               #,(make-swap #'s #'t)
                               (list s t)))))
	(main)))

(test '(1 2) 'srfi-72-example-2
      (let ((x 1))
	(let-syntax ((m (lambda (form)
			  (let ((x 2))
			    #`(list x #,x)))))
	  (m))))

(test '(1 3) 'srfi-72-example-3
      (let ((x 1))
	(let-syntax ((m (lambda (form)
			  (let ((x (car '(3))))
                            #`(list x #,x)))))
          (m))))

;; From R6RS, except [...] replaced by (...), and
;; using letrec-syntax instead of nested define-syntax.
(test '(#t #f)
      'free-identifier-1
      (let ((fred 17))
        (letrec-syntax
            ((a (lambda (x)
                  (syntax-case x ()
                    ((_ id) #'(b id fred)))))
             (b (lambda (x)
                  (syntax-case x ()
                    ((_ id1 id2)
                     #`(list
                           #,(free-identifier=? #'id1 #'id2)
                           #,(bound-identifier=? #'id1 #'id2)))))))
          (a fred))))

(begin
  ;; Note we need to compile define and define-for-syntax
  ;; in the same compilation unit for it to make sense.
  (define x-72-x3 1)
  (define-for-syntax x-72-x3 2)
  (test '(1 2) 'srfi-72-example-4
	(let-syntax ((m (lambda (form)
			  #`(list x-72-x3 #,x-72-x3))))
          (m))))

;; Based on Savannah bug #17984 Chris Wegrzyn <chris.wegrzyn@gmail.com>
;; Compile time error in expansion of hygienic macros ending in literals
(define thisfails
  (letrec-syntax
      ((outer
	(syntax-rules ()
	  ((outer expr)
	   (begin expr "this fails")))))
    (outer "third")))
(test "this fails" 'savannah-bug-17984 thisfails)

;; Savannah bug #18504 Margus Freudenthal <margus@cyber.ee>
;; Cannot generate (define-simple-class) using syntax-case macros
(define-syntax aa
  (lambda (x)
    (syntax-case x ()
      ((_ cl arg argtype)
       #`(define-simple-class cl ()
           (arg type: argtype))))))
(aa MyClass myparam <String>)
(define aa-instance ::MyClass (MyClass myparam: "sarg"))
(test (as <String> "sarg") 'savannah-bug-18504 aa-instance:myparam)

;; Savannah bug #18105: Chris Wegrzyn <chris.wegrzyn@gmail.com>
;; with-syntax causes NullPointerException during compilation but not in repl
(begin
  (define-syntax crashing-syntax
    (lambda (x)
      (syntax-case x ()
	((k args e1)
	 (with-syntax ((bodye1 (syntax e1)))
		      (syntax
		       (lambda args (begin bodye1))))))))
  (test 3 'savannah-bug-18105 ((crashing-syntax (arg1 arg2) 3) 1 2)))

;; Luis Casillas <luis@casillas.org> posted to Kawa list 2007-02-02:
(define-for-syntax (alter-syntax-datum proc stx)
  ;; must use define-syntax-datum in PLT
  (datum->syntax-object stx (proc (syntax-object->datum stx))))
(define-syntax define-symbol-altering-macro
  (syntax-rules ()
    ((_ (macro-name arg) expr . exprs)
     (define-symbol-altering-macro macro-name (lambda (arg) expr . exprs)))
    ((_ macro-name proc)
     (define-syntax macro-name
       (lambda (stx)
         (syntax-case stx ()
           ((_ sym . args)
            (let ((new-sym (alter-syntax-datum proc (syntax sym))))
              #`(#,new-sym . args)))))))))
(define-symbol-altering-macro (call-reversename sym)
  (string->symbol
   (list->string
    (reverse
     (string->list
      (symbol->string sym))))))
(test 7 'symbol-altering-macro (call-reversename xam 3 2 7 6))

;; Based on a bug reported by Dan Stanger <DStanger@EatonVance.Com>.
(define (test-literal-capture-1)
  (letrec-syntax
      ((define-input
	 (syntax-rules () ((define-input var) (define var 1))))
       (test-out
	(syntax-rules (test-content-type! define-input)
	  ((test-out (test-content-type! expr ...)) (test-content-type! expr ...))
	  ((test-out (define-input form)) (error (define-input form)))
	  ((test-out expr) (list expr)))))

    (test-out (symbol->string (quote b)))))
(test '("b") test-literal-capture-1)

;; A test submitted by Felix Klock <felix_klock_ii@mac.com>
(define (test-literal-capture-2)
  (let-syntax ((testm1 (syntax-rules (local-macro)
                         ((testm1 (local-macro e))  `((lit1 ,e)))
                         ((testm1 e)                `((els1 ,e))))))
    (append
     (testm1 'in-a)
     (testm1 (local-macro  'in-b))
     (let-syntax ((local-macro (syntax-rules ()
                                 ((local-macro e) `((2 ,e))))))
       (append
        (testm1 (local-macro  'in-c))
        (let-syntax ((testm2 (syntax-rules (local-macro)
                               ((testm2 (local-macro e))
                                `((lit2 ,e)))
                               ((testm2 e)
                                `((els2 ,e))))))
          (testm2 (local-macro  'in-d)))
        (let-syntax ((testm3 (syntax-rules (local-macro)
                               ((testm3 (local-macro e))
                                `((lit3 ,e)))
                               ((testm3 e)
                                `((els3 ,e)))))
                     (local-macro (syntax-rules ()
                                    ((local-macro e)
                                     `((4 ,e))))))
          (testm3 (local-macro 'in-e)))
        (let-syntax ((testm5 (syntax-rules (local-macro)
                               ((testm5 (local-macro e))
                                `((lit5 ,e)))
                               ((testm4 e)
                                `((els5 ,e))))))
          (let-syntax ((local-macro (syntax-rules ()
                                      ((local-macro e)
                                       `((6 ,e))))))
            (testm5 (local-macro 'in-f))))
        (let-syntax ((local-macro (syntax-rules ()
                                    ((local-macro e)
                                     `((7 ,e))))))
          (let-syntax ((testm8 (syntax-rules (local-macro)
                                 ((testm8 (local-macro e))
                                  `((lit8 ,e)))
                                 ((testm8 e)
                                  `((els8 ,e))))))
            (testm8 (local-macro 'in-g))))
        )))))
(test '((els1 in-a) (lit1 in-b) (els1 ((2 in-c))) (lit2 in-d) (els3 ((4 in-e))) (els5 ((6 in-f))) (lit8 in-g))
      test-literal-capture-2)

;; Savannah bug #26993 "String literals in syntax-rules don't match".
(define-syntax foo-26993
   (syntax-rules ()
     ((foo-26993 "foo") 'ok)))
(test 'ok 'test-savannah-26993 (foo-26993 "foo"))

;; Savannah bug #27042: Bad interaction between syntax-rules and call-with-values
;; (Though the was actually in the hygiene handling of lambda,
;; and had nothing to do specifically with call-with-values.)
(test '(0 10 0) 'test-savannah-27042
      (let-syntax ((dlet
		    (syntax-rules ()
		      ((dlet (var val) body)
		       (let ((saved var))
			 (set! var val)
			 (call-with-values (lambda () body)
			   (lambda (result)
			     (set! var saved)
			     result)))))))
	(let* ((x 0)
	       (x0 x)
	       (x1 
		(dlet (x (+ x 10))
		      x))
	       (x2 x))
	  (list x0 x1 x2))))

(test '(2 1) 'r7rs-test1
      (let ((x 1) (y 2))
        (define-syntax swap!
          (syntax-rules ()
            ((swap! a b)
             (let ((tmp a))
               (set! a b)
               (set! b tmp)))))
        (swap! x y)
        (list x y)))

(define-syntax r7rs-rec1
  (lambda (x)
    (syntax-case x ()
      ((_ x e)
       (identifier? #'x)
       #'(letrec ((x e)) x))
      ((_ x e)
       "not an identifier"))))
(test '(1 2 6 24 120) 'r7rs-rec1
      (map (r7rs-rec1 fact
                      (lambda (n)
                        (if (= n 0)                 
                            1
                            (* n (fact (- n 1))))))
           '(1 2 3 4 5)))
(test "not an identifier" 'r7rs-rec2
      (r7rs-rec1 5 (lambda (x) x))  )

(test '(#t #f) 'free-identifier-1
      (let ((fred 17))
        (define-syntax a
          (lambda (x)
            (syntax-case x ()
              ((_ id) #'(b id fred)))))
        (define-syntax b
          (lambda (x)
            (syntax-case x ()
              ((_ id1 id2)
               #`(list
                  #,(free-identifier=? #'id1 #'id2)
                  #,(bound-identifier=? #'id1 #'id2))))))
        (a fred)))
(test '(#t #f) 'free-identifier-2
      (let ((fred 17))
        (letrec-syntax
            ((a
              (lambda (x)
                (syntax-case x ()
                  ((_ id) #'(b id fred)))))
             (b
              (lambda (x)
                (syntax-case x ()
                  ((_ id1 id2)
                   #`(list
                           #,(free-identifier=? #'id1 #'id2)
                           #,(bound-identifier=? #'id1 #'id2)))))))
          (a fred))))

(define-syntax my-let
  (lambda (x)
    (define unique-ids?
      (lambda (ls)
        (or (null? ls)
            (and (let notmem? ((x (car ls)) (ls (cdr ls)))
                   (or (null? ls)
                       (and (not (bound-identifier=? x (car ls)))
                            (notmem? x (cdr ls)))))
                 (unique-ids? (cdr ls))))))
    (syntax-case x ()
      ((_ ((i v) ...) e1 e2 ...)
       (unique-ids? #'(i ...))
       #'((lambda (i ...) e1 e2 ...) v ...))
      ((_ . rest)
       "syntax error"))))

(test "syntax error" 'bound-identifier-1
      (my-let ((a 3) (a 4)) (+ a a)))
(test 7 'bound-identifier-2
      (my-let ((a 3) (b 4)) (+ a b)))
(test 7 'bound-identifier-3
      (let-syntax
          ((dolet (lambda (x)
                    (syntax-case x ()
                      ((_ b)
                       #'(my-let ((a 3) (b 4)) (+ a b)))))))
        (dolet a)))
;; For comparison, check with builtin let.
(test 7 'bound-identifier-4
      (let-syntax
          ((dolet (lambda (x)
                    (syntax-case x ()
                      ((_ b)
                       #'(let ((a 3) (b 4)) (+ a b)))))))
        (dolet a)))
;; Savannah bug #35552: bound-identifier=?
;; Note the SRFI-72 specifies the result #f, but MzScheme/Racket
;; and Chez Scheme both return #t.
(test #t 'bound-identifier-5
      (bound-identifier=? #'+ #'+))

(define-syntax my-case
  (lambda (x)
    (syntax-case x ()
      ((_ e0 ((k ...) e1 e2 ...) ...
          (else-key else-e1 else-e2 ...))
       (and (identifier? #'else-key)
            (free-identifier=? #'else-key #'else))
       #'(let ((t e0))
           (cond
            ((memv t '(k ...)) e1 e2 ...)
            ...
            (else else-e1 else-e2 ...))))
      ((_ e0 ((ka ...) e1a e2a ...)
          ((kb ...) e1b e2b ...) ...)
       #'(let ((t e0))
           (cond
            ((memv t '(ka ...)) e1a e2a ...)
            ((memv t '(kb ...)) e1b e2b ...)
            ...)))
      ((_ . rest)
       "syntax error"))))

(test "syntax error" 'my-case-1
      (let ((else #f))
        (my-case 0 (else (list "oops")))))
(test '("oops") 'my-case-2
      (let ((xy #f))
        (my-case 0 (else (list "oops")))))

;; Savannah bug report #35526, simplified version
(define-syntax foo-35526a
   (syntax-rules ()
     ((foo-35526a bar-id stuff ...)
      (let ((f (lambda () "+bar+")))
        (let-syntax ((bar-id (syntax-rules ()
                               ((bar-id) (f)))))
          (list stuff ...))))))
(define (baz-35526a)
  (foo-35526a bar (bar) (bar)))
(test '("+bar+" "+bar+") 'savannah-35526a (baz-35526a))

;; Savannah bug report #35526, original version
(define-syntax (foo-35526b form)
  (syntax-case form ()
    ((foo-id stuff ...)
     (with-syntax ((bar-id (datum->syntax (syntax foo-id) 'bar)))
                  (syntax
                   (let ((f (lambda () 'bar2)))
                     (let-syntax ((bar-id (syntax-rules ()
                                            ((bar-id) (f)))))
                       (list stuff ...))))))))
(define (baz-35526b)
  (foo-35526b (bar) (bar)))
(test '(bar2 bar2) 'savannah-35526b (baz-35526b))

;; #35555: Tail-call in syntax-case
(define (foo-35555 forms)
  (syntax-case forms ()
    (((x . y)  . rest)
     (foo-35555 #'rest))
    (() #t)
    (_ #f)))
(define-syntax bar-35555
  (lambda (forms)
    (foo-35555 (cdr forms))))
(test #t 'bar-35555-1 (bar-35555 (a b) (c d)))
(test #t 'bar-35555-2 (bar-35555))
(test #f 'bar-35555-3 (foo-35555 123))

;; Savannah bug report #39946 "NullPointerException when using syntax->datum"
(test '(+ 1 2) 'savannah-39946 (syntax->datum (syntax (+ 1 2))))

;; This example is in the Kawa internals documentation.
(define-syntax mac1
  (syntax-rules ()
    ((mac1-nest v1 init exp)
     (let ((v1 init))
       (let ((i 2))
         (list exp i))))))
(define j 10)
(test '(11 2) 'test-mac1 (mac1 i 1 (+ i j)))

;; Savannah bug report #40616 "Unhygienic syntax-rules"
(define-syntax def-a
  (syntax-rules ()
    ((_) (define a 'wrong))))
(test 'correct 'savannah-40616
      (let ((a 'correct)) (def-a) a))

(begin
  (define xlist '())
  (define-syntax def-b
    (syntax-rules ()
      ((_ val)
       (begin (define a val)
              (set! xlist (cons a xlist))))))
  (def-b 12)
  (def-b 42)
  (test '(42 12) 'savannah-40616-2 xlist))
