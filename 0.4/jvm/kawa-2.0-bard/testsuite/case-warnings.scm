(case #\a
  ((1 2 3 4 5) '1to5)
  ((6 7 8 9 10) '6to10)
  (else (display 'else) (newline)))
;; Diagnostic: case-warnings.scm:2:4: warning - datum type incompatible with the key
;; Diagnostic: case-warnings.scm:2:7: warning - datum type incompatible with the key
;; Diagnostic: case-warnings.scm:2:9: warning - there are 8 more datums that are incompatible with the key
;; Output: else

(format #t "~s~%" (case 10
                    ((1 2 3) 10)
                    ((4 5 6) 20)))
;; Diagnostic: case-warnings.scm:10:19: warning - missing else where value is required
;; Output: #!void

(define (c x :: int)
  (case x
    ((10000000000) 3)
    (((2 3 4 5)) 'list)
    ((2 3 4 5) 'num)
    ((#\a) #\a)
    ((sym) 54)
    (("a") 'a)
    (else 3)))

(display "list case: ")
(display (c 5))
(newline)
;; Diagnostic: case-warnings.scm:19:7: warning - List and vectors will never be matched in a case clause
;; Diagnostic: case-warnings.scm:19:7: warning - datum type incompatible with the key
;; Diagnostic: case-warnings.scm:21:6: warning - datum type incompatible with the key
;; Diagnostic: case-warnings.scm:22:6: warning - there are 2 more datums that are incompatible with the key
;; Output: list case: num

(define (d key)
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
             ((1 
                #(1 2 3 4)
               3) 'vect)
             ((5 6 7 8) 1)))))))))

(display "vector case: ")
(display (d 5))
(newline)
;; Diagnostic: case-warnings.scm:53:17: warning - List and vectors will never be matched in a case clause
;; Output: vector case: 1

;; key side effects

(case (begin (display "side effect, ") 35)
  ((1 2 3 4 5) '1to5)
  ((6 7 8 9 10) '6to10)
  (else (display 'else) (newline)))
;; Output: side effect, else

(define (tcase1 x)
   (case (begin (display "side effect, ") x)
     ((1 2 3 4 5) => (lambda (x) (format #t "c1 ~w~%" x)))
     ((6 7 8 9 10) '6to10)
     (else (display 'else) (newline))))
(tcase1 4)
;; Output: side effect, c1 4

(define (tcase2 x)
   (case (begin (display "side effect, ") x)
     ((1 2 3 4 5) (format #t "c1 ~w~%" x))
     ((6 7 8 9 10) '6to10)
     (else (display 'else) (newline))))
(tcase2 4)
;; Output: side effect, c1 4
