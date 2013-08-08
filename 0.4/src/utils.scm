;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          utils.scm
;;;; Project:       Bard
;;;; Purpose:       general utilities
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

(declare (standard-bindings)
         (extended-bindings)
         (inline)
         (inline-primitives))

;;; ----------------------------------------------------------------------
;;; simple utils
;;; ----------------------------------------------------------------------

(define (identity x) x)
(define (1+ n)(+ n 1))

;;; ----------------------------------------------------------------------
;;;  list utils
;;; ----------------------------------------------------------------------

(define (drop n ls)
  (list-tail ls n))

(define (take n ls)
  (let loop ((i 0)
             (ls* ls)
             (result '()))
    (if (< i n)
        (if (null? ls*)
            (error "count out of range in take: " n)
            (loop (+ i 1)
                  (cdr ls*)
                  (cons (car ls*)
                        result)))
        (reverse result))))

(define (last ls)
  (if (null? ls)
      (error "empty list")
      (if (null? (cdr ls))
          (car ls)
          (last (cdr ls)))))

(define (butlast ls)
  (let ((len (length ls)))
    (take (1- len) ls)))

;;; ----------------------------------------------------------------------
;;;  vector utils
;;; ----------------------------------------------------------------------

(define (vector-for-each fn vec)
  (let ((len (vector-length vec)))
    (let loop ((i 0))
      (if (< i len)
          (let ((it (vector-ref vec i)))
            (fn it)
            (loop (+ i 1)))
          vec))))

(define (vector-map fn vec)
  (let* ((len (vector-length vec))
         (outvec (make-vector len #f)))
    (let loop ((i 0))
      (if (< i len)
          (let ((it (vector-ref vec i)))
            (vector-set! outvec i (fn it)))))
    outvec))
