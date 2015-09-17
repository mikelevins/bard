(display "Memoization test 1: ")

(define s1 (delay (begin (display 'hello) 1)))
(force s1)
(force s1)
(newline)
;; Output: Memoization test 1: hello

;=========================================================================
(display "Memoization test 2: ")
(let ((s (delay (begin (display 'bonjour) 2))))
  (+ (force s) (force s)))
(newline)
;; Output: Memoization test 2: bonjour

;=========================================================================
(display "Memoization test 3: ") ; (pointed out by Alejandro Forero Cuervo) 
(define r3 (delay (begin (display 'hi) 1)))
(define s3 (lazy r3))
(define t3 (lazy s3))
(force t3)
(force r3)
(newline)
;; Output: Memoization test 3: hi

(display "Memoization test 3c: ")
(define-constant r3c (delay (begin (display 'hi) 1)))
(define-constant s3c (lazy r3c))
(define-constant t3c (lazy s3c))
(force t3c)
(force r3c)
(newline)
;; Output: Memoization test 3c: hi

;=========================================================================
(display "Memoization test 4: Stream memoization ")
(define (stream-drop s index)
  (lazy
   (if (zero? index)
       s
       (stream-drop (cdr (force s)) (- index 1)))))

(define (ones)
  (delay (begin
           (display 'ho)
           (cons 1 (ones)))))

(define s4 (ones))

(car (force (stream-drop s4 4)))
(car (force (stream-drop s4 4)))
(newline)
;; Output: Memoization test 4: Stream memoization ho ho ho ho ho

(display "Done.") (newline)
;; Output: Done.

;=================================================================
(display "Reentrancy test 1: from R5RS ")

(define count 0)
(define p5
  (delay (begin (set! count (+ count 1))
                (if (> count x5)
                    count
                    (force p5)))))
(define x5 5)
(display (force p5))                     ;===>  6
(set! x5 10)
(display (force p5))                     ;===>  6
(newline)
;; Output: Reentrancy test 1: from R5RS 6 6

;=========================================================================
(display "Reentrancy test 2: from SRFI 40: ")

(define f6
  (let ((first? #t))
    (delay
      (if first?
          (begin
            (set! first? #f)
            (force f6))
          'second))))

(display (force f6))                     ;===> 'second 
(newline)
;; Output: Reentrancy test 2: from SRFI 40: second

;=========================================================================
(display "Reentrancy test 3 - due to John Shutt: ")

(define q7
  (let ((count 5))
    (define (get-count) count)
    (define p7 (delay (if (<= count 0)
                         count
                         (begin (set! count (- count 1))
                                (force p7)
                                (set! count (+ count 2))
                                count))))
    (list get-count p7)))
(define get-count (car q7))
(define p7 (cadr q7))

(display (get-count))  ; =>   5
(display (force p7))    ; =>   0
(newline)
;; Output: Reentrancy test 3 - due to John Shutt: 5 0
