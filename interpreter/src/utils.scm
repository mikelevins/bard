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

;;;---------------------------------------------------------------------
;;; general utilities
;;;---------------------------------------------------------------------

(define (identity x) x)

(define (every? pred ls)
  (or (null? ls)
      (and (pred (car ls))
           (every? pred (cdr ls)))))

(define (any? pred ls)
  (if (null? ls)
      #f
      (or (and (pred (car ls)) (car ls))
          (any? pred (cdr ls)))))

(define (partial fn . args)
  (lambda more-args
    (apply fn `(,@args ,@more-args))))

(define (complement fn . args)
  (lambda more-args
    (not (apply fn `(,@args ,@more-args)))))

(define (range x y)
  (letrec ((aux (lambda (x y acc)
                  (if (>= x y)
                      (reverse acc)
                      (aux (+ x 1) y (cons x acc))))))
    (aux x y '())))

(define (copy-tree x)
  (let recur ((x x))
    (if (not (pair? x)) x
        (cons (recur (car x)) (recur (cdr x))))))

(define (position-if pred ls)
  (letrec ((aux (lambda (pred ls i)
                  (if (null? ls)
                      #f
                      (if (pred (car ls))
                          i
                          (aux pred (cdr ls) (+ i 1)))))))
    (aux pred ls 0)))

(define (remove item ls test)
  (letrec ((rm (lambda (item ls test acc)
                 (if (null? ls)
                     (reverse acc)
                     (if (test item (car ls))
                         (rm item (cdr ls) test acc)
                         (rm item (cdr ls) test (cons (car ls) acc)))))))
    (rm item ls test '())))

(define (plist->alist plist #!optional (acc '()))
  (if (null? plist)
      acc
      (let ((k (car plist)))
        (if (null? (cdr plist))
            (error "Malformed property list: " quals)
            (if (assq k acc)
                (error "Duplicate key in property list: " k)
                (let ((v (cadr plist)))
                  (plist->alist (cddr plist) (cons (cons k v) acc))))))))

(define (get-entry alist key pred default)
  (let loop ((entries alist))
    (if (null? entries)
        default
        (if (pred key (car (car entries)))
            (car entries)
            (loop (cdr entries))))))

(define (get alist key pred default)
  (let loop ((entries alist))
    (if (null? entries)
        default
        (if (pred key (car (car entries)))
            (cdr (car entries))
            (loop (cdr entries))))))

(define (filter pred ls)
  (letrec ((aux (lambda (pred ls acc)
                  (if (null? ls)
                      (reverse acc)
                      (if (pred (car ls))
                          (aux pred (cdr ls) (cons (car ls) acc))
                          (aux pred (cdr ls) acc))))))
    (aux pred ls '())))

(define (merge base-alist new-alist)
  (append (filter (lambda (entry) (not (assq (car entry) new-alist)))
                            base-alist) 
          new-alist))

(define (merge-rejecting-duplicates base-alist new-alist)
  (append (map (lambda (entry) 
                 (if (assq (car entry) new-alist)
                     (error "Duplicate key: " (car entry))
                     entry))
               base-alist) 
          new-alist))

(define (reduce fn base-val more-vals)
  (if (null? more-vals)
      base-val
      (reduce fn (fn base-val (car more-vals)) (cdr more-vals))))

;;; From Sort.scm
;;; Copyright (c) 2006-2008 by Marc Feeley, All Rights Reserved.
;;; slightly modified by mikel evins

(define (sort sequence less?)
  (define (sort-list lst less?)
    (define (mergesort lst)
      (define (merge lst1 lst2)
        (cond ((not (pair? lst1))
               lst2)
              ((not (pair? lst2))
               lst1)
              (else
               (let ((e1 (car lst1)) (e2 (car lst2)))
                 (if (less? e1 e2)
                     (cons e1 (merge (cdr lst1) lst2))
                     (cons e2 (merge lst1 (cdr lst2))))))))
      (define (split lst)
        (if (or (not (pair? lst)) (not (pair? (cdr lst))))
            lst
            (cons (car lst) (split (cddr lst)))))
      (if (or (not (pair? lst)) (not (pair? (cdr lst))))
          lst
          (let* ((lst1 (mergesort (split lst)))
                 (lst2 (mergesort (split (cdr lst)))))
            (merge lst1 lst2))))
    (mergesort lst))
  (cond ((not (procedure? less?))
         (error "procedure expected"))
        ((or (null? sequence)
             (pair? sequence))
         (sort-list sequence less?))
        ((vector? sequence)
         (list->vector (sort-list (vector->list sequence) less?)))
        (else
         (error "vector or list expected"))))

(define (not-yet-implemented . args)
  (error "Not yet implemented" args))

(define (->table . k/v-plist)
  (let ((tbl (make-table test: eq?)))
    (let loop ((kvs k/v-plist))
      (if (null? kvs)
          tbl
          (if (null? (cdr kvs))
              (error "odd number of inits in make-table" k/v-plist)
              (begin
                (table-set! tbl (car kvs)(cadr kvs))
                (loop (cddr kvs))))))))
