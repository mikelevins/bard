;;;; ***********************************************************************
;;;;
;;;; Name:          kernel.scm
;;;; Project:       Bard
;;;; Purpose:       the kernel evaluator
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(declare (extended-bindings))

;;; ----------------------------------------------------------------------
;;; list utils
;;; ----------------------------------------------------------------------

(define (filter test ls)
  (if (null? ls)
      '()
      (let ((next (car ls)))
        (if (test next)
            (cons next
                  (filter test (cdr ls)))
            (filter test (cdr ls))))))


(define (first expr)
  (car expr))

(define (interpose thing ls)
  (let loop ((elts ls)
             (result '()))
    (if (null? elts)
        (reverse result)
        (if (null? (cdr elts))
            (loop '()
                  (cons (car elts)
                        result))
            (loop (cdr elts)
                  (cons thing
                        (cons (car elts)
                              result)))))))
