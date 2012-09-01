;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          utils.scm
;;;; Project:       Bard
;;;; Purpose:       general-purpose utilities
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(define (take n ls)
  (let loop ((n n)
             (ls ls)
             (result '()))
    (if (<= n 0)
        (reverse result)
        (if (null? ls)
            (error "Count out of range")
            (loop (- n 1)
                  (cdr ls)
                  (cons (car ls) result))))))

(define (drop n ls)
  (let loop ((n n)
             (result ls))
    (if (<= n 0)
        result
        (loop (- n 1)
              (cdr result)))))
