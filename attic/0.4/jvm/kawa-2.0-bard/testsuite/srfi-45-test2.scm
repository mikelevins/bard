; Run: kawa -f testsuite/srfi-45-test2.scm -e '(leak-test-1)'
; and so on up to '(leak-test-7)
; Should run contunuously without running out of memory or stack space;
; (leak-test-6) and (leak-test-7) do finish after running a while.
; If run as-is, does finish in a short time.

;=========================================================================
; Test leaks:  All the leak tests should run in bounded space.

;=========================================================================
; Leak test 1: Infinite loop in bounded space.

(define (loop) (lazy (loop)))
(define (leak-test-1)
  (force (loop)))
;(leak-test-1)

;=========================================================================
; Leak test 2: Pending memos should not accumulate 
;              in shared structures.

(define s2 (loop))
(define (leak-test-2)
  (force s2))
;(leak-test-2)
;=========================================================================
; Leak test 3: Safely traversing infinite stream.

(define (from n)
  (delay (cons n (from (+ n 1)))))

(define (traverse s)
  (lazy (traverse (cdr (force s)))))

(define (leak-test-3)
  (force (traverse (from 0))))               ;==> bounded space
;(leak-test-3)
;=========================================================================
; Leak test 4: Safely traversing infinite stream 
;              while pointer to head of result exists.

(define s4 (traverse (from 0)))  
(define (leak-test-4)
  (force s4))                                  ;==> bounded space
;(leak-test-4)
;=========================================================================
; Convenient list deconstructor used below.

(define-syntax match
  (syntax-rules ()
    ((match exp 
       (()      exp1)
       ((h . t) exp2))
     (let ((lst exp))
       (cond ((null? lst) exp1)
             ((pair? lst) (let ((h (car lst))
                                (t (cdr lst)))
                            exp2))
             (else 'match-error))))))

;========================================================================
; Leak test 5: Naive stream-filter should run in bounded space.
;              Simplest case.

(define (stream-filter p? s)
  (lazy (match (force s)
          (()      (delay '())) 
          ((h . t) (if (p? h)
                       (delay (cons h (stream-filter p? t)))
                       (stream-filter p? t))))))

(define (leak-test-5)
  (force (stream-filter (lambda (n) (= n 10000000000))
                        (from 0))))
;(leak=test-5)                                         ;==> bounded space

;========================================================================
; Leak test 6: Another long traversal should run in bounded space.

; The stream-ref procedure below does not strictly need to be lazy.  
; It is defined lazy for the purpose of testing safe compostion of 
; lazy procedures in the times3 benchmark below (previous 
; candidate solutions had failed this).  

(define (stream-ref s index)
  (lazy
   (match (force s)
     (()      'error)
     ((h . t) (if (zero? index)
                  (delay h)
                  (stream-ref t (- index 1)))))))

; Check that evenness is correctly implemented - should terminate:

(force (stream-ref (stream-filter zero? (from 0))
                   0))                              ;==> 0

(display (force (stream-ref (from 0) 200))) (newline)
;; Output: 200
(define s6 (stream-ref (from 0) 100000000))
(define (leak-test-6)
  (force s6))                                         ;==> bounded space
;(leak-test-6)

;======================================================================
; Leak test 7: Infamous example from SRFI 40. 

(define (times3 n)
  (stream-ref (stream-filter
               (lambda (x) (zero? (modulo x n)))
               (from 0))
              3))

(display (force (times3 7)))
(newline)
;; Output: 21

(define (leak-test-7)
  (force (times3 100000000)))                     ;==> bounded space
;(leak-test-7)

(display "Ok.") (newline)
;; Output: Ok.
