;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          utils.scm
;;;; Project:       Bard
;;;; Purpose:       general utilities
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

;;; ---------------------------------------------------------------------
;;; list utils
;;; ---------------------------------------------------------------------

(define (drop n ls)
  (let loop ((i n)
             (items ls))
    (if (<= i 0)
        items
        (if (null? ls)
            (error (str "index out of range: " n))
            (loop (- i 1) (cdr items))))))

;;; ---------------------------------------------------------------------
;;; string utils
;;; ---------------------------------------------------------------------

(define (str . args)
  (if (null? args)
      ""
      (let ((s (if (string? (car args))
                   (car args)
                   (object->string (car args)))))
        (if (null? (cdr args))
            s
            (string-append s (apply str (cdr args)))))))

;;; ---------------------------------------------------------------------
;;; vector utils
;;; ---------------------------------------------------------------------

(define (vector-map proc vec)
  (list->vector (map proc (vector->list vec))))

(define (vector-position item vec #!key (test eq?))
  (let ((len (vector-length vec)))
    (let loop ((i 0))
      (if (< i len)
          (if (test item (vector-ref vec i))
              i
              (loop (+ i 1)))
          #f))))

