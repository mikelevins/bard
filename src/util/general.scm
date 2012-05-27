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

(define (identity x) x)

(define (constantly x) (lambda args x))

(define (partial fn . args)
  (lambda more-args
    (apply fn `(,@args ,@more-args))))

(define (flip fn)(lambda (x y)(fn y x)))

(define (complement fn . args)
  (lambda more-args
    (not (apply fn `(,@args ,@more-args)))))


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

(define (interpose item ls)
  (if (or (null? ls)
          (null? (cdr ls)))
      ls
      (cons (car ls)
            (cons item
                  (interpose item (cdr ls))))))

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
