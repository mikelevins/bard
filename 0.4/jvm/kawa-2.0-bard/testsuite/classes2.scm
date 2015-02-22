(require <classes1>)

;; Test separate compilation of type alias of non-simple class.
;; Might as well make it a forward declaration.
(define-alias cls-d <ClsD>)

(define-class <ClsD> (<ClsB>)
  (d :: <int> init-form: 23))
;  ((f (y :: <int>)) :: <int> (+ d y)))

(define-class <ClsE> (<ClsC> <ClsD>)
  (e :: <int> init-form: 39)
  ((f (y :: int)) ::int  throws: (java.lang.Exception)
   (+ 100 xx  e y)))

(define-class-using-syntax-rules <SimpleC> <SimpleB>
  (d :: <int> init-form: 23 init-keyword: d:)
  (e :: <int> init-form: 24))

(define (make-simpleC ival) (make <SimpleC> i: ival))

(define yy :: <int> 56)

(define date-test-instance (make <DateTest>))

(define-namespace date-test-ns <SimpleDateTest>)
(define (make-date-test)
  (let ((d :: <SimpleDateTest> (date-test-ns:new)))
    (date-test-ns:get-year d)))

;; Test that we can make <TestCapture1> before <TestCapture1> is defined.
(define (make-TestCapture1)
  (make <TestCapture1>))

(define-simple-class <TestCapture1> ()
  (z :: <integer> init: 11)
  ((ff farg)
   (list
    (lambda (y) (list yy z y)))))

(define-simple-class <TestCapture2> ()
  (z :: <integer> init: 12)
  ((ff farg)
   (list
    (lambda (y) (list yy y)))))

;; Test for Savannah bug #15151
(define (getClassTest o) (slot-ref o 'class))

(define (classes2-capture-test-a x)
  (let ((obj
	 (object ()
		 (action (list x (lambda (e) (- yy 10)))))))
    (slot-ref obj 'action)))

(define (classes2-capture-test-b x)
  (let ((obj
	 (object ()
		 (y)
		 (action (lambda (e) (+ yy 10))))))
    (slot-ref obj 'action)))

(define my-id-instance-2 :: my-id-class-2
  (let ((ii :: my-id-class-2 (my-id-class-2)))
    ii))
