
(test-begin "case")

;; special cases

(test 1 (lambda (key)
          (case key
            (else 1)))
      5)

(test 1 (lambda (key)
          (case key
            ((5) 1)))
      5)

(test 1 (lambda (key)
          (case key
            ((5) 1)
            (else 2)))
      5)

(test 2 (lambda (key)
          (case key
            ((5) 1)
            (else 2)))
      4)

(test #!void (lambda (key)
               (case key
                 ((5) 1)
                 ((3) 2)))
      4)

;; integer datums, integer key

(test 1 (lambda (key::byte)
          (case key
            ((1 2 3 4) 3)
            ((5 6 7 8) 1)))
      5)

(test 1 (lambda (key::byte)
          (case key
            ((1 2 3 4) 3)
            ((6 7 8) 2)
            (else 1)))
      5)


(test 1 (lambda (key::short)
          (case key
            ((1 2 3 4) 3)
            ((5 6 7 8) 1)))
      5)

(test 1 (lambda (key::short)
          (case key
            ((1 2 3 4) 3)
            ((6 7 8) 2)
            (else 1)))
      5)


(test 1 (lambda (key::int)
          (case key
            ((1 2 3 4) 3)
            ((5 6 7 8) 1)))
      5)

(test 1 (lambda (key::int)
          (case key
            ((1 2 3 4) 3)
            ((6 7 8) 2)
            (else 1)))
      5)

(test 5 (lambda (key::int)
          (case key
            ((1) 1)
            ((2) 2)
            ((3) 3)
            ((4) 4)
            ((5) 5)
            ((6) 6)
            ((7) 7)
            ((8) 8)
            ((9) 9)
            ((10) 10)
            ((11) 11)
            ((12) 12)))
      5)

(test 1 (lambda (key::long)
          (case key
            ((1 2 3 4) 3)
            ((5 6 7 8) 1)))
      5)

(test 1 (lambda (key::long)
          (case key
            ((1 2 3 4) 3)
            ((6 7 8) 2)
            (else 1)))
      5)


;; integer datums, object key

(test 1 (lambda (key)
          (case key
            ((1 2 3 4) 3)
            ((5 6 7 8) 1)))
      5)

(test 1 (lambda (key)
          (case key
            ((1 2 3 4) 3)
            ((6 7 8) 2)
            (else 1)))
      5)

(test 5 (lambda (key)
          (case key
            ((1) 1)
            ((2) 2)
            ((3) 3)
            ((4) 4)
            ((5) 5)
            ((6) 6)
            ((7) 7)
            ((8) 8)
            ((9) 9)
            ((10) 10)
            ((11) 11)
            ((12) 12)))
      5)

;; big integer datums, integer key

(test 12 (lambda (key::int)
           (case key
             ((-2147483636) 1)
             ((-2147483637) 2)
             ((-2147483638) 3)
             ((-2147483639) 4)
             ((-2147483640) 5)
             ((-2147483641) 6)
             ((-2147483642) 7)
             ((-2147483643) 8)
             ((-2147483644) 9)
             ((-2147483645) 10)
             ((-2147483646) 11)
             ((-2147483647) 12)))
      -2147483647)

(test 1 (lambda (key::int)
          (case key
            ((-2147483636) 1)
            ((-2147483637) 2)
            ((2147483638) 3)
            ((2147483639) 4)
            ((2147483640) 5)
            ((2147483641) 6)
            ((2147483642) 7)
            ((2147483643) 8)
            ((-2147483644) 9)
            ((2147483645) 10)
            ((2147483646) 11)
            ((2147483647) 12)))
      -2147483636)

(test 1 (lambda (key::int)
          (case key
            ((2147483636) 1)
            ((2147483637) 2)
            ((2147483638) 3)
            ((2147483639) 4)
            ((2147483640) 5)
            ((2147483641) 6)
            ((2147483642) 7)
            ((2147483643) 8)
            ((2147483644) 9)
            ((2147483645) 10)
            ((2147483646) 11)
            ((2147483647) 12)))
      2147483636)

;; big integer datums, object key

(test 12 (lambda (key)
          (case key
            ((2147483636) 1)
            ((2147483637) 2)
            ((2147483638) 3)
            ((2147483639) 4)
            ((2147483640) 5)
            ((2147483641) 6)
            ((2147483642) 7)
            ((2147483643) 8)
            ((2147483644) 9)
            ((2147483645) 10)
            ((2147483646) 11)
            ((2147483647) 12)))
      2147483647)

;; char datums, char key
(test 1 (lambda (key::char)
          (case key
            ((#\a #\b #\c #\d) 3)
            ((#\e #\f #\g #\h) 1)))
      #\e)

(test 1 (lambda (key::char)
          (case key
            ((#\a #\b #\c #\d) 3)
            ((#\e #\f #\g #\h) 2)
            (else 1)))
      #\z)

;; integer datums, arrow syntax
(test 5 (lambda (key)
          (case key
            ((1 2 3 4) 3)
            ((5 6 7 8) => (lambda (x) x))
            (else 1)))
      5)

(test 5 (lambda (key)
          (case key
            ((1 2 3 4) 3)
            ((6 7 8) 2)
            (else => (lambda (x) x))))
      5)

(test 5 (lambda (key)
          (case key
            ((1 2 3 4) 3)
            ((5 6 7 8) => (lambda (x) x))
            (else => (lambda (x) (+ x 1)))))
      5)

(test 5 (lambda (key)
          (case key
            ((1 2 3 4) => (lambda (x) x))
            ((5 6 7 8) => (lambda (x) x))
            (else => (lambda (x) (+ x 1)))))
      5)

;; mixed types
(test 5 (lambda (key)
          (case key
            ((#\s) (display 1))
            ((s foo bar) (display 2) (display 3))
            ((5) => (lambda (x) x))
            ((6) (lambda (x)(display x)))
            (else (lambda (x)(display x)))))
      5)

(test 'sym (lambda (key)
             (case key
               ((10000000000) 3)
               ((2 3 4 5) 'sym)
               ((#\a) #\a)
               ((sym) 54)
               (("a") 'a)
               (else 3)))
      5)

;; mixed types, same expression 
;; in some clauses
(test 'sym (lambda (key)
             (case key
               ((10000000000) (cons key '()) 'sym)
               ((2 3 4 5) (cons key '()) 'sym)
               ((#\a) #\a)
               ((sym) 54)
               (("a") (cons key '()) 'sym)
               (else 3)))
      5)

;; nested cases

(test 1 (lambda (key)
          (case key
            ((1 2 3 4)
             (case key
               ((2 3 4) 3)
               ((1 5 6 7 8)
                (case key
                  ((1 2 3 4) 1)
                  ((5 6 7 8) 1)))))
            ((5 6 7 8) 1)
            ((9 10 11 12)
             (case key
               ((1 2 3 4)
                (case key
                  ((1 2 3 4) 3)
                  ((5 6 7 8)
                   (case key
                     ((1 2 3 4) 3)
                     ((5 6 7 8) 1)))))))))
      1)

(test 2 (lambda (key)
          (case key
            ((1) 1)
            ((2)
             (case key
               ((4) 4)
               ((3) 3)
               ((5) 5)
               ((2)
                (let ((k #\c))
                  (case k
                    ((#\a) 4)
                    ((#\b) 3)
                    ((#\c) 2)
                    ((#\d) 5))))))
            (else 'error)))
      2)

;; hashcode collisions

(test 'a (lambda (key)
           (let ((k (case key
                      ((4) 'a)
                      ((3) #\b)
                      (else 200))))
             (case k
               ((97) 97)
               ((#\a) #\a)
               ((a) 'a))))
      4)

(define (test-collision-same-branch x)
  (case (list-ref
         '(#\a #\b c #\d e #\f 103 #\h #\i 106)
         (mod x 10))
    ((#\a a 97) 0)
    ((#\b b 98) 1)
    ((#\c c 99) 2)
    ((#\d d 100) 3)
    ((#\e e 101) 4)
    ((#\f f 102) 5)
    ((#\g g 103) 6)
    ((#\h h 104) 7)
    ((#\i i 105) 8)
    ((#\j j 106) 9)
    (else 'error)))

(define (test-collision-different-branch x)
  (case (list-ref
         '(#\a 98 99 d a #\b c 100 97 b #\c #\d)
         (mod x 12))
    ((#\a) 0)
    ((#\b) 5)
    ((#\c) 10)
    ((#\d) 11)
    ((a) 4)
    ((b) 9)
    ((c) 6)
    ((d) 3)
    ((97) 8)
    ((98) 1)
    ((99) 2)
    ((100) 7)
    (else 'error)))

(define (case-loop x end-val f)
  (when (< x end-val)
    (begin
      (test x f x)
      (case-loop (+ x 1) end-val f))))

(case-loop 0 10 test-collision-same-branch)
(case-loop 0 12 test-collision-different-branch)

;; inside letrec

(test 2 (lambda ()
          (letrec ((f (lambda (x)
                        (if (eqv? x 1)
                            (g x)
                            (h x))))
                   (g (lambda (x)
                        (h x)))
                   (h (lambda (x)
                        (case x
                          ((0) (h x))
                          ((2) x)))))
            (f 2))))

