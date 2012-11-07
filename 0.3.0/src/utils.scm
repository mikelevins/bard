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
;;; Lists
;;; ---------------------------------------------------------------------

(define (copy-alist alist)
  (map (lambda (p)(cons (car p)(cdr p)))
       alist))

(define (drop n ls)
  (list-tail ls n))

(define (interpose val items)
  (if (or (null? items)
          (null? (cdr items)))
      items
      (cons (car items)
            (cons val (interpose val (cdr items))))))

(define (plist->alist plist)
  (if (null? plist)
      '()
      (if (null? (cdr plist))
          (error (str "Malformed plist: " plist))
          (cons (cons (car plist)
                      (cadr plist))
                (plist->alist (cddr plist))))))

(define (remove-if pred ls)
  (if (null? ls)
    '()
    (if (pred (car ls))
      (remove-if pred (cdr ls))
      (cons (car ls)
        (remove-if pred (cdr ls))))))

;;; ---------------------------------------------------------------------
;;; Strings
;;; ---------------------------------------------------------------------

(define (->str arg)
  (if (string? arg)
      arg
      (object->string arg)))

(define (str . args)
  (if (null? args)
      ""
      (apply string-append (map ->str args))))

;;; ---------------------------------------------------------------------
;;; Vectors
;;; ---------------------------------------------------------------------

(define (vector-position item vec #!key (test eq?))
  (let loop ((i 0))
    (if (>= i (vector-length vec))
        #f
        (let ((it (vector-ref vec i)))
          (if (test item it)
              i
              (loop (+ i 1)))))))