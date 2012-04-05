;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          list.scm
;;;; Project:       Bard
;;;; Purpose:       list utilities
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(define (every? pred . lss)
  (if (null? lss)
      #t
      (if (any? null? lss)
          #t
          (if (apply pred (map car lss))
              (apply every? (cons pred (map cdr lss)))
              #f))))

(define (any? pred ls)
  (if (null? ls)
      #f
      (or (and (pred (car ls)) (car ls))
          (any? pred (cdr ls)))))

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

(define (drop n ls)
  (if (<= n 0)
      ls
      (drop (- n 1) (cdr ls))))

(define (take n ls)
  (if (<= n 0)
      '()
      (cons (car ls)
            (take (- n 1)(cdr ls)))))

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
  (if (null? ls)
      '()
      (if (pred (car ls))
          (cons (car ls)
                (filter pred (cdr ls)))
          (filter pred (cdr ls)))))

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
