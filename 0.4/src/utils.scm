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

(define (drop n ls)
  (let ((itemcount (length ls)))
    (if (<= n itemcount)
        (let loop ((i n)
                   (items ls))
          (if (<= i 0)
              items
              (loop (- i 1)
                    (cdr items))))
        (error "Too few elements" ls))))

(define (filter test ls)
  (if (null? ls)
      '()
      (let ((next (car ls)))
        (if (test next)
            (cons next
                  (filter test (cdr ls)))
            (filter test (cdr ls))))))

(define first car)
(define rest cdr)
(define second cadr)
(define third caddr)
(define fourth cadddr)

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

(define (position-if test ls)
  (let loop ((i 0)
             (items ls))
    (if (null? items)
        #f
        (if (test (car items))
            i
            (loop (+ 1 i)
                  (cdr items))))))

(define (zip left-list right-list)
  (let loop ((keys left-list)
             (vals right-list)
             (result '()))
    (if (or (null? keys)
            (null? vals))
        (reverse result)
        (loop (cdr keys)
              (cdr vals)
              (cons (cons (car keys)
                          (car vals))
                    result)))))

;;; ----------------------------------------------------------------------
;;; string utils
;;; ----------------------------------------------------------------------

(define (match-prefix? pref str)
  (not-yet-implemented 'match-prefix?))

(define (trim-whitespace str)
  (not-yet-implemented 'trim-whitespace))

