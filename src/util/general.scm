;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          general.scm
;;;; Project:       Bard
;;;; Purpose:       uncategorized utilities
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************


(define (not-yet-implemented . args)
  (error "Not yet implemented" args))

(define (identity x) x)

(define (constantly x) (lambda args x))

(define (partial fn . args)
  (lambda more-args
    (apply fn `(,@args ,@more-args))))

(define (flip fn)(lambda (x y)(fn y x)))

(define (complement fn . args)
  (lambda more-args
    (not (apply fn `(,@args ,@more-args)))))

(define (iota n)
  (let loop ((i (- n 1))
             (result '()))
    (if (<= i 0)
        (cons i result)
        (loop (- i 1)
              (cons i result)))))

(define (bound? name)
  (not (##unbound? (##global-var-ref (##make-global-var name)))))

(define (plist->alist plist)
  (let loop ((kvs plist)
             (result '()))
    (if (null? kvs)
        (reverse result)
        (if (null? (cdr kvs))
            (error (string-append "Malformed plist: " (object->string plist)))
            (loop (cddr kvs)
                  (cons (cons (car kvs)(cadr kvs))
                        result))))))

(define (alist-keys alist)(map car alist))
(define (alist-vals alist)(map cdr alist))

(define (alist-get alist key #!key (default #f)(test equal?))
  (let loop ((alist alist))
    (if (null? alist)
        default
        (if (test key (car (car alist)))
            (cdr (car alist))
            (loop (cdr alist))))))

(define (every? fn ls)
  (let loop ((items ls))
    (if (null? items)
        #t
        (if (fn (car items))
            (loop (cdr items))
            #f))))

(define (interpose item ls)
  (if (or (null? ls)
          (null? (cdr ls)))
      ls
      (cons (car ls)
            (cons item
                  (interpose item (cdr ls))))))

(define (remove-if test ls)
  (let loop ((items ls))
    (if (null? items)
        '()
        (if (test (car items))
            (remove-if test (cdr items))
            (cons (car items)
                  (remove-if test (cdr items)))))))

(define (slib:error . args)
  (error (apply string-append (interpose " " (map object->string args)))))

(define (vector-every? fn vec)
  (let ((len (vector-length vec)))
    (let loop ((i 0))
      (if (>= i len)
          #t
          (if (fn (vector-ref vec i))
              (loop (+ i 1))
              #f)))))

(define (getf key plist #!optional (default #f))
  (let ((tl (member key plist)))
    (and tl (not (null? (cdr tl))) (cadr tl))))

(define (drop n ls)
  (list-tail ls n))

(define (take n ls)
  (let loop ((ls ls)
             (n n))
    (if (<= n 0)
        '()
        (if (null? ls)
            (error "count out of range")
            (cons (car ls)
                  (loop (cdr ls)
                        (- n 1)))))))


