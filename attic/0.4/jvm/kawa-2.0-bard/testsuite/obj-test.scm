(test-init "Objects" 143)

;; Force procedure to be applied without being inlined:
(define-syntax force-eval
  (syntax-rules () ((force-eval proc arg ...)
                    (apply proc (list arg ...)))))

;; Force call to be compiled with (hopefully) inlining:
(define-syntax force-compile
  (syntax-rules () ((force-compile proc arg ...)
                    ((lambda (#!key keydummy) (proc arg ...))))))

(define complex (make-record-type "complex" '(re im)))
(define make-complex (record-constructor complex))
(define z (make-complex 3 4))
(define make-rcomplex (record-constructor complex '(im re)))
(test z make-rcomplex 4 3)
(test 4 'accessor1 ((record-accessor complex 'im) z))
((record-modifier complex 're) z 5)
(test z make complex im: 4 re: 5)
(test 5 'accessor2 ((record-accessor complex 're) z))
(test #t record? z)
(test #f record? 5)
(test #t 'record-predicate ((record-predicate complex) z))
(test complex record-type-descriptor z)
(test "complex" record-type-name complex)
(test '(re im) record-type-field-names complex)

(test 20 'set! (begin (set! z:im 15) (+ z:re z:im)))

;; Check name mangling and demangling of records.
(define funny-record (make-record-type 'lispy-name->foo!? '(a! b-c)))
(test "lispy-name->foo!?" record-type-name funny-record)
(test '(a! b-c) record-type-field-names funny-record)
(define make-funny-record1 (record-constructor funny-record))
(define make-funny-record2 (record-constructor funny-record '(b-c a!)))
(define lt1 (make-funny-record1 10 12))
(test 10 'accessor21 ((record-accessor funny-record 'a!) lt1))
((record-modifier funny-record 'b-c) lt1 (+ 2 lt1:b-c))
(set! lt1:a! 9)
(test (make-funny-record2 14 9) 'funny-record lt1)

(test '(10 "10" 20 "20") 'object-with-field-1
       (let*
	   ((obj (object (<object>)
			 (fld 10)
			 ((toString) <String> fld)))
	    (val1 (field obj 'fld))
	    (str1 (as <String> obj)))
	 (begin
	   (set! (field obj 'fld) 20)
	   (let*
	       ((val2 (field obj 'fld))
		(str2 (as <String> obj)))
	     (list val1 str1 val2 str2)))))

(test '(100 "100" 20 "20") 'object-with-field-2
       (let*
	   ((val0 100)
	    (obj (object (<object>)
			 (fld val0)
			 ((toString) <String> fld)))
	    (val1 (field obj 'fld))
	    (str1 (as <String> obj)))
	 (begin
	   (set! (field obj 'fld) 20)
	   (let*
	       ((val2 (field obj 'fld))
		(str2 (as <String> obj)))
	     (list val1 str1 val2 str2)))))

(test 1 'object-locals
      (let ((x (object (<java.lang.Object>) (z (lambda (x) (display x)))))) 1))

(test 1 'object-locals
      (let* ((d display)
	     (x (object (<java.lang.Object>) (z (lambda (x) (d x)))))) 1))

(test 2 'object-with-closure-1
      (length (let*
		  ((name 'x)
		   (obj (object (<java.util.zip.Adler32>))))
		(letrec ((opt
			  (lambda (args)
			    (list obj
				  (object (<java.lang.Object>
					   <java.awt.event.ItemListener>)
					  ((itemStateChanged
					    (arg <java.awt.event.ItemEvent>))
					   <void>
					   (display name) 
					   (newline)))))))
		  (opt 3)))))

(define (object-with-closure-2 c-name)
  (let* ((c-path (symbol->string c-name)) 
	 (c-obj (object (<java.lang.Object>)))) 
    (letrec ((opt (lambda (args) 
		    (if (pair? args) 
			(begin 
			  (let ((listener
				 (object (<java.lang.Object>
					  <java.awt.event.ItemListener>) 
					 ((itemStateChanged (arg <java.awt.event.ItemEvent>)) 
					  <void> 
					  (display "listener of checkbutton ") 
					  (display c-name) 
					  (display arg) 
					  (newline))))) 
			    (list c-obj listener)) 
			  (opt (cddr args)))))))
      (opt (list )))
    c-path))

(test ".x.c" object-with-closure-2 '.x.c)

(define (document-filter arg1)
  (lambda (arg2)
    (object ()
            ((toString)
             <String>
             (format #f "{arg1: ~s arg2: ~s}" arg1 arg2)))))

(test "{arg1: 23 arg2: 12}" 'object-with-closure-3
      (as <String> ((document-filter 23) 12)))

(define i100 (force-eval make <integer> ival: 100))
(define i200 (force-compile make <integer> ival: 200))
(test 100 'test-make-1 i100)
(test 200 'test-make-2 i200)
(define cons1 (force-eval make <pair> 7 9))
(test '(7 . 9) 'test-make-3 cons1)
(test '(9 . 6) 'test-make-3 (force-compile make <pair> 9 6))
(force-eval slot-set! cons1 'cdr 99)
(test '(7 . 99) 'test-slot-set-1 cons1)
(force-compile slot-set! cons1 'cdr (field '(88 99) 'car))
(test '(7 . 88) 'test-slot-set-2 cons1)
(set! (slot-ref cons1 'car) 8)
(test '(8 . 88) 'test-slot-set-3 cons1)
(set! (field cons1 'cdr) 55)
(test '(8 . 55) 'test-slot-set-3 cons1)
(test #t 'test-slot-ref-1
      (force-eval static-field <java.lang.Boolean> 'TRUE))
(test '() 'test-slot-ref-2
      (force-compile static-field <list> 'Empty))
(test #t 'test-slot-ref-3
      (force-eval field #f 'TRUE))
(test #f 'test-slot-ref-4
      (force-compile field #t 'FALSE))

;; Make sure objects compiled in separate compilations don't cause
;; errors (naming clashes)
(test '(1 2) (lambda ()
               ;; define each object in a separate compile
               (define obj1 (eval '(object () ((one) 1))))
               (define obj2 (eval '(object () ((two) 2))))
               (list (invoke obj1 'one)
                     (invoke obj2 'two))))

(define-variable internal-node-name list)
(require <module2>)
(test "fun2" fun1)
(test "fun1" 'fun2 (fun2))
(test '("fun1" "fun2") fun1fun2)
(test 4 list-length-1 '(a b c d))
(test 2 list-length-5 '(a b))
(test 0 length (classify))

(test 3 length-diff1 "abcdef" "abc")
(test 3 length-diff2 'abcdef 'abc)
(test 3 length-diff3 'abcdef 'abc)

(test '(1 2 3 4) 'deldup-test list1234)

;; Test bug reported by Jocelyn Paine.
(test '(boolean #t) make-literal #t)

(test '(3 . 4) make-pair 3 4)

(test 7 'my-array-length (field my-array-7 'length))

(define-variable dvar1 2)
(define-variable dvar2 3)
(require <module3>)

(test 13 'Savannah-bug-40822 (macro2))

(let* ((all0 (all-zeros))
       (nlen (gnu.lists.LList:listLength all0 #t)))
  (test "#0=(0 . #0#) len:-1" 'Savannah-bug-43233
        (format #f "~w len:~d" all0 nlen)))


(define-variable dvar3 4)
(test '(2 3 13) 'dvar-test-1 dvar-test-1)
(set! dvar1 1)
(test '(1 3 13) list dvar1 dvar2 dvar3)
(test 0 list-length-4 '())
(test '(10 24 11 190) test1-import0)
(test '(24 15 11 181 #(15 338 169)) test3-import1)
(test '(11 111) 'test-mod0-v2
      (let ((v2 (get3-mod0-v2)))
	(set3-mod0-v2 (+ 100 v2))
	(list v2 (get3-mod0-v2))))
(test 25 'test-mod2-v5  mod2-v5)

;; Test for Savannah bug #34004: Nullpointer exception in compiler
(test 1 check-thunk)

(test '(1 2) 'counter-test-result counter-test-result)

(define ts1 (make <MyTimestamp> 10 1))
(define ts2 (make <MyTimestamp> 10 2))
(test #t < (my-compare ts1 ts2) 0)

;; Based on Savannah bug#11822, contributed by Dean Ferreyra.
;; (Other parts of this testcase are in module1.scm and module3.scm.)
(mB <NewClass> 'simple-sym)
(test '(100 simple-sym) 'bug-11822
      (let ((s :: <simpleAux> (make <simpleAux>))
	    (nc :: <NewClass> (make <NewClass>)))
	(invoke nc 'fn s)))

(test 24 'factorial-4 factorial-4)

(test 1062806400000 'namespace-syntax-test (namespace-syntax-call))

(test 'Z check-fluid-let 'Z)

(define IsClass2-value (make <IdClass2>))
(require <classes1>)
(require <classes2>)

(test 3 slot-ref IsClass2-value 'var1)
(test 4 slot-ref IsClass2-value 'var2)
(set! fail-expected "static counter is incorrectly initialized in non-static run method")
(test 5 get-new-count)

(define obj1 (make <SimpleA>))
(test 4 slot-ref obj1 'a)
(test 6 slot-ref obj1 'b)
(test 35 'obj1-f (invoke obj1 'f 5))
(test #(y) 'lambda-method1 ((invoke obj1 'lambda-method1) 'y))
(test #(z z z) 'lambda-method2 ((invoke obj1 'lambda-method2 -1) 'z))
(test 2 'lambda-method3 (invoke obj1 'lambda-method3))
((invoke obj1 'lambda-method5 13))
(test 13 'lambda-method4 (invoke obj1 'lambda-method4))
((invoke obj1 'lambda-method6) 12)
(test 12 'lambda-method4 (invoke obj1 'lambda-method4))
((invoke obj1 'lambda-method7) 2)
(test 2 'lambda-method4 (invoke obj1 'lambda-method4))
(test '(nn mm (r1 r2)) 'lambda-method-rest1
      ((invoke obj1 'lambda-method-rest1 'nn) 'mm 'r1 'r2))
(test "TroubleIdentity" 'test-trouble ((invoke obj1 'trouble) "Trouble"))
(slot-set! obj1 'a (+ 10 (static-field <SimpleA> 'b)))
(test "yes" slot-ref obj1 'hyphenated-field?)
(test 16 field obj1 'a)
(slot-set! obj1 'happy #t)
(test #t slot-ref obj1 'happy)
(slot-set! obj1 'happy #f)
(test #f slot-ref obj1 'happy)
(force-compile slot-set! (as <SimpleA> obj1) 'happy #t)
(test #t slot-ref (as <SimpleA> obj1) 'happy)
(set! (field obj1 'happy) #f)
(test #f field obj1 'happy)
(test "a:5 b:(10 15)" 'with-var-arg (obj1:withVarArg 5 10 15))

(define obj2 (make <SimpleB>))
(test 4 field obj2 'a)
(test 6 field obj2 'b)
(test 6 static-field <SimpleB> 'b)
(test 10 slot-ref obj2 'c)
(test 1045 'obj2-f (invoke obj2 'f 15))

(define obj3 (make <SimpleC> d: 25))
(test 4 'obj3-a (slot-ref obj3 'a))
(test 6 'obj3-b (slot-ref obj3 'b))
(test 10 'obj3-c (slot-ref obj3 'c))
(test 25 slot-ref obj3 'd)
(test 24 slot-ref obj3 'e)
(test 3 slot-ref (make-simpleC 3) 'i)

(define obj4 (make <ClsC>))
(test 14 'obj4-b (slot-ref obj4 'b))
(test 22 'obj4-c (slot-ref obj4 'c))
(test 44 'obj4-f (invoke obj4 'f 2))

(define (make-ClsD) (make <ClsD>))
(define obj5 (let ((x ::cls-d (make-ClsD))) x))
(test 23 'obj5-d (slot-ref obj5 'd))

(define obj6 (make <ClsE>))
(set! (field obj6 'e) (- (field obj6 'e) 10))
(test 29 'obj6-e (slot-ref obj6 'e))
(test 156 'obj6-f (invoke obj6 'f 7))

(define test-capture-1 (make-TestCapture1))
(test '(56 11 21) 'test-capture-1 ((car (invoke test-capture-1 'ff 99)) 21))
(define test-capture-2 (make <TestCapture2>))
(test '(56 22) 'test-capture-2 ((car (invoke test-capture-2 'ff 99)) 22))

(test 46 'classes2-capture-test-a
      ((cadr (classes2-capture-test-a 100)) 'a))
(test 66 'classes2-capture-test-a
      ((classes2-capture-test-b 100) 'b))

(require <MyFunc>)
(test '(1 2 3) my-func-1 2 3)
(require <MyModule>)
(test '(#t 5 6) my-func-t 5 6)

(define TestCapturedFieldRef-instance (<TestCapturedFieldRef> 7))
(incr-field-function 2)
(test 109 'test-captured-field-ref TestCapturedFieldRef-instance:var)

(define-record-type pare
  (kons x y-part)
  pare?
  (x kar set-kar!)
  (y-part kdr))
(test #t pare? (kons 1 2))
(test #f pare? (cons 1 2))
(test 1 kar (kons 1 2))
(test 2 kdr (kons 1 2))
(test 3 'set-kdr!
      (let ((k (kons 1 2)))
	(set-kar! k 3)
	(kar k)))

(define obj-with-let
  (object ()
	  ((meth v)
	   (let ((n v) (r (this)))
	     (list n)))) )
(test '(3) 'ff (invoke obj-with-let 'meth 3))

(define (create-main-frame)
  (object ()
   (myField init:
        (format #f "this: ~a\n" (this))
        )))
(define main-frame (create-main-frame))
(test (format #f "this: ~a\n" main-frame) field main-frame 'myField)

(define simple-date (make <SimpleDateTest>))
(define-namespace date "class:java.util.Date")
(test (+ 1900 (date:get-year (date:new)))
      invoke simple-date 'get-year)
(define non-simple-date (make <DateTest>))
(test (+ 1900 (*:get-year (date:new)))
      invoke non-simple-date 'get-year)
(test #t 'date-1 (>= (invoke date-test-instance 'get-year) 2004))
(test #f 'date-2 (invoke non-simple-date 'before date-test-instance))
(test #t 'date-3 (>= (make-date-test) 2004))

(test 13 'colon-test-1 (list 11 12 13 14):cdr:cdr:car)
(test '(12 13 14) 'colon-test-2 (list 11 12 13 14):cdr)
(define colon-test-list-1 (list 11 12 13 14))
(test 13 'colon-test-3 colon-test-list-1:cdr:cdr:car)
(test '(12 13 14) 'colon-test-4 colon-test-list-1:cdr)
(test "(11 12 13 14)" 'colon-test-5 (colon-test-list-1:toString))
(test "(12 13 14)" 'colon-test-6 (colon-test-list-1:cdr:toString))

;; Test for Savannah bug #4289
(define pa-data (pa-new 10))
(pa-setter pa-data 5 99)
(test 99 pa-getter pa-data 5)
(test #!null pa-getter pa-data 6)
(test 10 pa-length pa-data)

;; Test for Savannah bug #5651
(define arr-5 ((primitive-array-new <int>) 5))
((primitive-array-set <int>) arr-5 1 98)
(iarr-set arr-5 0 99)
(test '(99 98 0) 'prim-arr-test
      (let ((getter (primitive-array-get <int>)))
	(list (getter arr-5 0) (getter arr-5 1)
	      ((primitive-array-get <int>) arr-5 4))))

;; Test for Savannah bug #15151
(test (*:toString "gnu.math.IntNum") 'getClassTest
      (*:getName (getClassTest '123)))

(require <cycle1>)
(test #t is-even? 8)
(test #f is-even? 3)
(define c1-n1 (cycle1-name1))
(define c1-n2 (cycle1-name2))
(define c1-n3 (cycle1-name3))
(test "cycle1" 'check-reference-module-name
      (and (eq? c1-n1 c1-n2) (eq? c1-n1 c1-n3)
	   (java.lang.Class? c1-n1) (invoke c1-n1 'getName)))
(require <cycle2>)
(test #t is-even? 8)
(test #f is-even? 3)
(test #f is-odd? 8)
(test #t is-odd? 3)

(define m2-object (module2))
(test '(#t #t #f #t) 'check-m2-types
     (list (module2? m2-object)
	   (pair? m2-object)
	   (java.lang.Appendable? m2-object)
	   (java.io.Closeable? m2-object)))
(set-cdr! m2-object '(5 6 7))
(test 4 length m2-object)
(test #!null 'before-m2-close m2-object:my-array-7)
(let ((cl ::java.io.Closeable m2-object)) (cl:close))
(test #f 'after-m2-close m2-object:my-array-7)
