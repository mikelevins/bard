;; The next line is just to test the case that we don't extend ModuleBody.
(module-extends <pair>)
(define-alias Closeable java.io.Closeable)
(module-implements Closeable)
(module-export list-length-1 list-length-3 classify list1234 <A8711>
	       length-diff1 length-diff2 length-diff3 make-literal make-pair
	       my-array-7 mod2-v5 close abc-returner
               (rename fun1 fun2) (rename fun2 fun1) fun1fun2
               (rename list-length-3 list-length-5))
(require <module1>)
(require <module1a>)
(define (list-length-1 x) :: <integer>
  (list-length-2 x))
(define list-length-3 #t)
(set! list-length-3 list-length-2)

(define (fun1) "fun1")
(define (fun2) "fun2")
(define (fun1fun2) (list (fun1) (fun2)))

(define list1234 (deldup (call-to-first '((1 1 2 3 4 4)))))

;; Caused VerifyError
(define-variable internal-node-name)
(define name internal-node-name)
(define (classify)
  (let node-loop ((classes '()))
    (map (lambda (class)
	   (lambda (node1)
	     (name node1)))
	 classes)))

;; Based on a bug report from Walter C. Pelissero <walter@pelissero.org>:
(define (length-diff1 (str1 :: jlString)
		      (str2 :: <java.lang.String>))
  (let ((diff (- (invoke str1 'length) (invoke str2 'length))))
    diff))
(define (length-diff2 (str1 :: <String>)
		      (str2 :: <String>))
  (let ((diff :: <int> (- (invoke str1 'length) (invoke str2 'length))))
    diff))
(define (length-diff3 (str1 :: <String>)
		      (str2 :: <String>)) :: <int>
  (let ((diff :: <int> (- (invoke str1 'length) (invoke str2 'length))))
    diff))

;; Testcase from Jocelyn Paine
(define (make-boolean-literal (boolean <boolean>)) :: <list>
  (list 'boolean boolean))
(define (make-literal value) :: <list>
  (cond
   ((boolean? value) (make-boolean-literal value))
   (else (format #t "Illegal value: ~A~%" value))))

(define (make-pair x y)
  (make <pair> car: x cdr: y))

;; Used to cause stack overflow - see bug #8818.
(define-abc-func abc-returner)

;; Based on Savannah bug #8711, from Dominique Boucher
;; If I remove the (module-export <A>) clause or add <B> to the exported
;; variables, everything works fine. 
(define-simple-class <A8711> (<java.lang.Object>)
  ((execute (x :: <int>) (y :: <int>) (z :: <int>) (w :: <int>)) :: <void>
   #!void))
(define-simple-class <B8711> (<A8711>)
  (objects type: <list> init-keyword: objects:)
  ((execute (x :: <int>) (y :: <int>) (z :: <int>) (w :: <int>)) :: <void>
   (for-each
    (lambda (object :: <A8711>)
      (invoke object 'execute x y z y))
    objects)))

(define my-array-7 (make-array-fun 7))

;; Derived from Savannah bug# 14640.
;; (The key is that mod0-v5 is imported and otherwise unreferenced in this
;; module.  Thus CAN_READ didn't get set for mod0-v5, which confused things.)
(define mod2-v5
  (let ((loc (location mod1-v5)))
    (+ 10 (loc))))

(define (close) :: void
  (set! my-array-7 #f))
