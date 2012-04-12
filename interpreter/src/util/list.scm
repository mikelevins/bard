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

(define (any? pred ls)
  (if (null? ls)
      #f
      (or (and (pred (car ls)) (car ls))
          (any? pred (cdr ls)))))

(define (copy-tree x)
  (let recur ((x x))
    (if (not (pair? x)) x
        (cons (recur (car x)) (recur (cdr x))))))

(define (drop n ls)
  (if (<= n 0)
      ls
      (if (null? ls)
          (error "index out of range" n)
          (drop (- n 1) (cdr ls)))))

(define (drop-before pred ls)
  (if (null? ls)
      '()
      (if (pred (car ls))
          ls
          (drop-before pred (cdr ls)))))

(define (every? pred . lss)
  (if (null? lss)
      #t
      (if (any? null? lss)
          #t
          (if (apply pred (map car lss))
              (apply every? (cons pred (map cdr lss)))
              #f))))

(define (filter pred ls)
  (if (null? ls)
      '()
      (if (pred (car ls))
          (cons (car ls)
                (filter pred (cdr ls)))
          (filter pred (cdr ls)))))

(define (get alist key pred default)
  (let loop ((entries alist))
    (if (null? entries)
        default
        (if (pred key (car (car entries)))
            (cdr (car entries))
            (loop (cdr entries))))))

(define (get-entry alist key pred default)
  (let loop ((entries alist))
    (if (null? entries)
        default
        (if (pred key (car (car entries)))
            (car entries)
            (loop (cdr entries))))))

(define (interleave l1 l2)
  (if (null? l1)
      '()
      (if (null? l2)
          '()
          (cons (car l1)(cons (car l2) (interleave (cdr l1)(cdr l2)))))))

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

(define (nub ls)
  (let loop ((elts ls)
             (result '()))
    (if (null? elts)
        (reverse result)
        (if (member (car elts) result)
            (loop (cdr elts) result)
            (loop (cdr elts) (cons (car elts) result))))))

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

(define (position-if pred ls)
  (letrec ((aux (lambda (pred ls i)
                  (if (null? ls)
                      #f
                      (if (pred (car ls))
                          i
                          (aux pred (cdr ls) (+ i 1)))))))
    (aux pred ls 0)))

(define (range x y)
  (letrec ((aux (lambda (x y acc)
                  (if (>= x y)
                      (reverse acc)
                      (aux (+ x 1) y (cons x acc))))))
    (aux x y '())))

(define (reduce fn base-val more-vals)
  (if (null? more-vals)
      base-val
      (reduce fn (fn base-val (car more-vals)) (cdr more-vals))))

(define (remove item ls test)
  (letrec ((rm (lambda (item ls test acc)
                 (if (null? ls)
                     (reverse acc)
                     (if (test item (car ls))
                         (rm item (cdr ls) test acc)
                         (rm item (cdr ls) test (cons (car ls) acc)))))))
    (rm item ls test '())))

(define (take n ls)
  (if (<= n 0)
      '()
      (if (null? ls)
          (error "index out of range" n)
          (cons (car ls)
                (take (- n 1)(cdr ls))))))

(define (take-before pred ls)
  (if (null? ls)
      '()
      (if (pred (car ls))
          '()
          (cons (car ls)
            (take-before pred (cdr ls))))))

