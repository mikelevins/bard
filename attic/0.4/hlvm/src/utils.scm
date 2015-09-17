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
;;; function utils
;;; ----------------------------------------------------------------------

(define (complement proc)
  (lambda args
    (not (apply proc args))))

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

(define (take n ls)
  (let loop ((i n)
             (items ls)
             (result '()))
    (if (< i 1)
        (reverse result)
        (if (null? items)
            (error "Count out of range: " n)
            (loop (- i 1)
                  (cdr items)
                  (cons (car items)
                        result))))))

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
  (let loop ((i 0))
    (if (< i (string-length str))
        (if (< i (string-length pref))
            (if (char=? (string-ref pref i)
                        (string-ref str i))
                (loop (+ 1 i))
                #f)
            #t)
        (string=? pref str))))

(define (string-position-if test str)
  (let loop ((i 0))
    (if (< i (string-length str))
        (if (test (string-ref str i))
            i
            (loop (+ i 1)))
        #f)))

(define (string-right-position-if test str)
  (let loop ((i (- (string-length str) 1)))
    (if (>= i 0)
        (if (test (string-ref str i))
            (+ 1 i)
            (loop (- i 1)))
        #f)))

(define (trim-whitespace-left str)
  (let ((pos (string-position-if (complement char-whitespace?) str)))
    (if pos
        (substring str pos (string-length str))
        "")))

(define (trim-whitespace-right str)
  (let ((pos (string-right-position-if (complement char-whitespace?) str)))
    (if pos
        (substring str 0 pos)
        "")))

(define (trim-whitespace str)
  (trim-whitespace-left (trim-whitespace-right str)))

