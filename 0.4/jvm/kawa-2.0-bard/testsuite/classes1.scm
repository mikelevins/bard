(module-static counter get-new-count <IdClass1> <IdClass2> my-id-class-2 call-lambda)

;; Based on a test-case from Jamison Hope <jrh@theptrgroup.com>
(define-syntax (import-class form)
  (syntax-case form ()
    ((import-class fqcn)
     (let* ((cls :: java.lang.Class (eval (syntax fqcn)))
	    (name (string->symbol (java.lang.Class:getSimpleName cls))))
       #`(define-alias #,(datum->syntax-object form name) fqcn)))))

(import-class java.util.Date)

(define-constant xx :: <int> 20)

(define-simple-class <SimpleA> ()
  (two :: <int> init-form: 2 access: 'private)
  (a :: <int> init: (- b two))
  (b :: <int> init-form: 6 allocation: class:)
  (n22 :: <int> init-value: 22 allocation: 'static access: 'protected)
  (hyphenated-field? init-value: "yes")

  (mHappy init: #t)
  ((isHappy) :: <boolean>
   mHappy)
  ((setHappy val :: <boolean>) :: <void>
   (set! mHappy val))

  ((lambda-method1)
   (lambda (x) (make-vector 1 x))) ; This lambda doesn't "capture" anything.
  ((lambda-method2 n)
   (lambda (x) (make-vector (+ a n) x))) ; This lambda does.
  ((lambda-method3)
   (call-lambda (lambda () (slot-ref (this) 'two))))
  ((lambda-method4)
   (call-lambda (lambda () two)))
  ((lambda-method5 x)
   (lambda () (set! two x)))
  ((lambda-method6)
   (lambda (x) (set! two x)))
  ((lambda-method7)
   (lambda (x) (slot-set! (this) 'two x)))
  ((lambda-method-rest1 name)
   (lambda (arg1 . rest) (list name arg1 rest)))
  ((x1900) :: <int> access: 'package allocation: 'static
   1900)
  ((g) :: <int> access: 'protected
   (+ xx a))
  ((f (y :: <int>)) :: <int>
   (if (equal? hyphenated-field? "yes") (+ (g) b y) 999))

  ((withVarArg a #!rest b)
   (format "a:~a b:~b" a b))

  ((asText (i ::int))::string (java.lang.Integer:toString i))
  ((asText (t ::java.lang.CharSequence))::string (t:toString))
  ((test-asText)
   (with-compile-options warn-invoke-unknown-method: #t warn-as-error: #t
			 ((this):asText "hello")))

  ;; Bug reported by Dean Ferreyra <dferreyra@igc.org> 2005-06-09
  ((trouble)
   (let ((fn (lambda (o)
               (get-identity-property-name o))))
     fn))
  ((get-identity-property-name property-name)
   (string-append property-name "Identity")))

(define (call-lambda fn)
  (fn))

(define-class <ClsB> ()
  (b :: <int> 14)) ;; deprecated syntax

(define-class <ClsC> (<ClsB>)
  (c :: <int>)
  (init: (set! c (static-field <SimpleA> 'n22)))
  ((f (y :: <int>)) :: <int> (+ xx c y)))

(define-syntax define-class-using-syntax-rules
  (syntax-rules ()
		((define-class-using-syntax-rules name super parts ...)
		 (define-simple-class name (super) parts ...))))

(define *MY-YEAR-OFFSET* 1900)
(define (default-offset)
  *MY-YEAR-OFFSET*)

(define-simple-class <DateTest> (Date)
  (offset init-form:  (default-offset))
  ((get-year) :: <int>
   ;; Saying plain Date below doesn't work - we get (this):getDate
   (+ (invoke-special java.util.Date (this) 'get-year) offset)))

(define-simple-class <SimpleDateTest> (<java.util.Date>)
  ((get-year) :: <int>
   (+ (invoke-special <java.util.Date> (this) 'get-year)
      (invoke-static <SimpleA> 'x1900))))

;; Test separate compilation of type-alias for simple class.
;; Make it a forward declaration for a better test.
(define-alias my-id-class-2 <IdClass2>)

(define-simple-class <IdClass1> ()
  (var0 allocation: 'class init: (get-new-count))
  (var1 init-form: (get-new-count)))

(define-private counter :: <int> 0)

(define (get-new-count)
  (set! counter (+ counter 1))
  counter)

(define-simple-class <IdClass2> (<IdClass1>)
  (allocation: 'class init: (get-new-count))
  (var2 init-form: (get-new-count)))

(module-static incr-field-function)
(define incr-field-function #f)
(define-simple-class <TestCapturedFieldRef> ()
  (var 100)
  ((*init* var0)
   (set! var (+ var var0))
   (set! incr-field-function
	 (lambda (delta)
	   (set! var (+ var delta))))))
