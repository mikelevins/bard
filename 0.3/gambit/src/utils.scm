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
;;; macros
;;; ---------------------------------------------------------------------

(define-macro (unless test . body)
  `(if (not ,test)
       (begin
         ,@body)))

;;; ---------------------------------------------------------------------
;;; general utils
;;; ---------------------------------------------------------------------

(define (identity x) x)

;;; ---------------------------------------------------------------------
;;; debugging utils
;;; ---------------------------------------------------------------------

(define (assert test msg)
  (unless test (error msg)))

;;; ---------------------------------------------------------------------
;;; value utils
;;; ---------------------------------------------------------------------

(define (identity x) x)

;;; ---------------------------------------------------------------------
;;; list utils
;;; ---------------------------------------------------------------------

(define (copy-tree ls)
  (if (pair? ls)
      (if (null? ls)
          ls
          (cons (copy-tree (car ls))
                (copy-tree (cdr ls))))
      ls))

(define (drop n ls)
  (let loop ((i n)
             (items ls))
    (if (<= i 0)
        items
        (if (null? ls)
            (error (str "index out of range: " n))
            (loop (- i 1) (cdr items))))))

(define (drop-while pred ls)
  (if (null? ls)
      ls
      (if (pred (car ls))
          (drop-while pred (cdr ls))
          ls)))

(define (every? pred ls)
  (if (null? ls)
      #t
      (if (pred (car ls))
          (every? pred (cdr ls))
          #f)))

(define (find-association key entries #!key (test equal?))
  (let loop ((entries entries))
    (if (null? entries)
        #f
        (if (test key (car entries))
            (car entries)
            (loop (cdr entries))))))

(define (interpose item ls)
  (if (or (null? ls)
          (null? (cdr ls)))
      ls
      (cons (car ls)
            (cons item
                  (interpose item (cdr ls))))))

(define (iota n)
  (let loop ((i 0)
             (ls '()))
    (if (<= n i)
        (reverse ls)
        (loop (+ 1 i)
              (cons i ls)))))

(define (position item ls #!key (test eq?))
  (let loop ((items ls)
             (i 0))
    (if (null? items)
        #f
        (if (test item (car items))
            i
            (loop (cdr items)
                  (+ i 1))))))

(define (remove test entries)
  (let loop ((entries entries))
    (if (null? entries)
        '()
        (if (test (car entries))
            (loop (cdr entries))
            (cons (car entries)
                  (loop (cdr entries)))))))

(define (set-nth-car! ls n v)
  (let loop ((i 0)
             (items ls))
    (if (null? items)
        (error "Index out of range")
        (if (= i n)
            (set-car! items v)
            (loop (+ 1 i)(cdr items))))
    ls))

(define (take n ls)
  (if (<= n 0)
      '()
      (cons (car ls)
            (take (- n 1)
                  (cdr ls)))))

(define (take-while pred ls)
  (if (null? ls)
      '()
      (if (pred (car ls))
          (cons (car ls)
                (take-while pred (cdr ls)))
          '())))

;;; ---------------------------------------------------------------------
;;; string utils
;;; ---------------------------------------------------------------------

(define (str . args)
  (if (null? args)
      ""
      (let ((s (if (string? (car args))
                   (car args)
                   (if (char? (car args))
                       (string (car args))
                       (object->string (car args))))))
        (if (null? (cdr args))
            s
            (string-append s (apply str (cdr args)))))))

;;; ---------------------------------------------------------------------
;;; vector utils
;;; ---------------------------------------------------------------------

(define (vector-for-each proc vec)
  (let ((len (vector-length vec)))
    (let loop ((i 0))
      (if (< i len)
          (begin
            (proc (vector-ref vec i))
            (loop (+ i 1)))
          vec))))

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

