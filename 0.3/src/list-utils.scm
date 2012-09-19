;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          list-utils.scm
;;;; Project:       Bard
;;;; Purpose:       general-purpose list utilities 
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(define (take n ls)
  (let loop ((elts ls)
             (n n)
             (result '()))
    (if (<= n 0)
        (reverse result)
        (if (null? elts)
            (error "count out of range")
            (loop (cdr elts)
                  (- n 1)
                  (cons (car elts)
                        result))))))

(define (drop n ls)
  (let loop ((elts ls)
             (n n))
    (if (<= n 0)
        elts
        (if (null? elts)
            (error "count out of range")
            (loop (cdr elts)(- n 1))))))


#| tests

take

(take 0 '())
(take 1 '()) ; should error
(take 0 '(0 1 2 3 4 5))
(take 1 '(0 1 2 3 4 5))
(take 3 '(0 1 2 3 4 5))
(take 6 '(0 1 2 3 4 5))
(take 7 '(0 1 2 3 4 5)) ; should error

drop

(drop 0 '())
(drop 1 '()) ; should error
(drop 0 '(0 1 2 3 4 5))
(drop 1 '(0 1 2 3 4 5))
(drop 3 '(0 1 2 3 4 5))
(drop 6 '(0 1 2 3 4 5))
(drop 7 '(0 1 2 3 4 5)) ; should error

|#
