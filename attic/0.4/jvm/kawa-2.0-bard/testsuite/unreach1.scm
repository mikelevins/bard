(define (bar1)
  (if (primitive-throw (java.lang.NullPointerException)) (list 23) 11))
;; Diagnostic: unreach1.scm:2:58: warning - unreachable code

(define (bar2)
  (list (primitive-throw (java.lang.NullPointerException)) 12 13))
;; Diagnostic: unreach1.scm:6:3: warning - unreachable procedure call
;; Diagnostic: unreach1.scm:6:9: note - this operand never finishes

(define (bar3)
  (list 12 (primitive-throw (java.lang.NullPointerException)) (sqrt 13)))
;; Diagnostic: unreach1.scm:11:3: warning - unreachable procedure call
;; Diagnostic: unreach1.scm:11:12: note - this operand never finishes

(define (bar4)
  (primitive-throw (java.lang.NullPointerException))
  13)
;; Diagnostic: unreach1.scm:17:3: warning - unreachable code

(define (bar5)
  (begin (primitive-throw (java.lang.NullPointerException))
         13))
;; Diagnostic: unreach1.scm:22:10: warning - unreachable code

;;; Savannah bug #35524: Unreachable code is not an error
(define (foo)
  (call-with-current-continuation
   (lambda (return)
     (let l ()
       (return #f)
       (l)))))
;; Diagnostic: unreach1.scm:31:8: warning - unreachable code

;;; Savannah bug #36560: eq? <endless-loop>
(define (foo36560)
  (eq? (let loop () (loop)) #t))
;; Diagnostic: unreach1.scm:36:3: warning - unreachable procedure call
;; Diagnostic: unreach1.scm:36:8: note - this operand never finishes

(define (let1 x y)
  (let ((a (list x y))
        (b (primitive-throw (java.lang.NullPointerException))))
    (list a b)))
;; Diagnostic: unreach1.scm:42:12: warning - initialization of b never finishes

(define (let2 x y)
  (fluid-let ((a (list x y))
              (b (primitive-throw (java.lang.NullPointerException))))
    (list a b)))
;; Diagnostic: unreach1.scm:48:18: warning - initialization of b never finishes

(define (let3 x y)
  (let ((a (primitive-throw (java.lang.NullPointerException)))
        (b (list x y)))
    (list a b)))
;; Diagnostic: unreach1.scm:53:12: warning - initialization of a never finishes

;;; Savannah bug #40123: Nested with-compile-options problem
(define (nested-with-compile-options)
  (with-compile-options warn-unreachable: #f
   (let ((one "one") (two "two"))
     (with-compile-options warn-unreachable: #f
      (if (primitive-throw (java.lang.NullPointerException)) one two)))
   (if (primitive-throw (java.lang.NullPointerException)) 1 2)))
(try-catch
 (begin (nested-with-compile-options)
        (format #t "nested-with-compile-options finished.~%"))
 (ex java.lang.NullPointerException
     (format #t "nested-with-compile-options threw NullPointerException.~%")))
;; Output: nested-with-compile-options threw NullPointerException.
 
(define loop (lambda () 
               (let loop ((n 1))
                 (loop (+ n 1)))))
(define (e x)
  (case x
    ((1) (loop))
    ((2 3 4 5) (loop))
    ((#\a) (loop))))

(try-catch (display (e 6)) 
  (exc java.lang.ClassCastException (lambda () (#!void))))
;; Diagnostic: unreach1.scm:81:12: warning - unreachable procedure call
;; Diagnostic: unreach1.scm:81:21: note - this operand never finishes

