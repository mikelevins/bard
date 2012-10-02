;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          list.scm
;;;; Project:       Bard
;;;; Purpose:       general-purpose list utils 
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(define (take n ls)
  (let loop ((i n)
             (items ls)
             (result '()))
    (if (<= i 0)
        (reverse result)
        (if (null? items)
            (error (str "Count out of range: " n))
            (loop (- i 1)
                  (cdr items)
                  (cons (car items)
                        result))))))
