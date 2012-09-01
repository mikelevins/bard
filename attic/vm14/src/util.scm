;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          util.scm
;;;; Project:       Bard
;;;; Purpose:       general-purpose utilities
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(define (str . args)
  (if (null? args)
      ""
      (let ((arg (if (string? (car args))
                     (car args)
                     (object->string (car args))))
            (more (cdr args)))
        (if (null? more)
            arg
            (string-append arg (apply str more))))))

(define (vector-position-if pred vec)
  (let ((len (vector-length vec)))
    (let loop ((i 0))
      (if (< i len)
          (if (pred (vector-ref vec i))
              i
              (loop (+ 1 i)))
          #f))))

(define (interpose expr items)
  (if (or (null? items)
          (null? (cdr items)))
      items
      (cons (car items)
            (cons expr (interpose expr (cdr items))))))
