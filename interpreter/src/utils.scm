;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          utils.scm
;;;; Project:       Bard
;;;; Purpose:       uncategorized utilities
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

;;; ---------------------------------------------------------------------
;;; file utils
;;; ---------------------------------------------------------------------

(define (read-file path)
  (call-with-input-file path
    (lambda (in)
      (apply string-append (interpose (string #\newline) (read-all in read-line))))))

;;; ---------------------------------------------------------------------
;;; system utils
;;; ---------------------------------------------------------------------

(define (assert test msg)
  (if test #t (error msg)))

(define (bound? name)
  (not (##unbound? (##global-var-ref (##make-global-var name)))))

(define (not-yet-implemented . args)
  (error "Not yet implemented" args))

(define (slib:error . args)
  (error (apply string-append (interpose " " (map object->string args)))))

;;; ---------------------------------------------------------------------
;;; functional utils
;;; ---------------------------------------------------------------------

(define (complement fn . args)
  (lambda more-args
    (not (apply fn `(,@args ,@more-args)))))

(define (constantly x) (lambda args x))

(define (identity x) x)

(define (flip fn)(lambda (x y)(fn y x)))

(define (iota n)
  (let loop ((i (- n 1))
             (result '()))
    (if (<= i 0)
        (cons i result)
        (loop (- i 1)
              (cons i result)))))

(define (partial fn . args)
  (lambda more-args
    (apply fn `(,@args ,@more-args))))

;;; ---------------------------------------------------------------------
;;; list utils
;;; ---------------------------------------------------------------------

(define (drop n ls)
  (list-tail ls n))

(define (every? fn ls)
  (let loop ((items ls))
    (if (null? items)
        #t
        (if (fn (car items))
            (loop (cdr items))
            #f))))

(define (filter test ls)
  (if (null? ls)
      '()
      (if (test (car ls))
          (cons (car ls)
                (filter test (cdr ls)))
          (filter test (cdr ls)))))

(define (getf key plist #!optional (default #f))
  (let ((tl (member key plist)))
    (and tl (not (null? (cdr tl))) (cadr tl))))

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

(define (some? fn ls)
  (let loop ((items ls))
    (if (null? items)
        #f
        (if (fn (car items))
            (car items)
            (loop (cdr items))))))

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

;;; ---------------------------------------------------------------------
;;; alist utils
;;; ---------------------------------------------------------------------

(define (alist-get alist key #!key (default #f)(test equal?))
  (let loop ((alist alist))
    (if (null? alist)
        default
        (if (test key (car (car alist)))
            (cdr (car alist))
            (loop (cdr alist))))))

(define (alist-keys alist)(map car alist))
(define (alist-vals alist)(map cdr alist))

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

;;; ---------------------------------------------------------------------
;;; string utils
;;; ---------------------------------------------------------------------

(define (comment-line? str)
  (let ((len (string-length str)))
    (let loop ((i 0))
      (if (< i len)
          (let ((ch (string-ref str i)))
            (if (char-whitespace? ch)
                (loop (+ i 1))
                (if (char=? #\; ch)
                    #t
                    #f)))
          #f))))

(define (empty-string? str)
  (> 1 (string-length str)))

(define (left-trim str test)
  (let ((len (string-length str)))
    (let loop ((i 0))
      (if (< i len)
          (let ((ch (string-ref str i)))
            (if (test ch)
                (loop (+ i 1))
                (substring str i len)))
          ""))))

(define (nonempty-source-line? str)
  (and (string? str)
       (not (comment-line? str))
       (not (empty-string? (trim-whitespace str)))))


(define (right-trim str test)
  (let loop ((i (- (string-length str) 1)))
    (if (< i 0)
        ""
        (let ((ch (string-ref str i)))
          (if (test ch)
              (loop (- i 1))
              (substring str 0 (+ i 1)))))))

(define (string-char-position ch str)
  (let ((len (string-length str)))
    (let loop ((i 0))
      (if (< i len)
          (if (char=? ch (string-ref str i))
              i
              (loop (+ 1 i)))
          #f))))

(define (string-split-on ch str)
  (let loop ((instr str)
             (result '()))
    (if (< (string-length instr) 1)
        (reverse result)
        (let ((chpos (string-char-position ch instr)))
          (if chpos
              (let ((seg (substring instr 0 chpos))
                    (outstr (substring instr (+ 1 chpos) (string-length instr))))
                (loop outstr (cons seg result)))
              (loop "" (cons instr result)))))))

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


(define (trim-whitespace str)
  (right-trim (left-trim str char-whitespace?) char-whitespace?))

;;; ---------------------------------------------------------------------
;;; vector utils
;;; ---------------------------------------------------------------------

(define (vector-every? fn vec)
  (let ((len (vector-length vec)))
    (let loop ((i 0))
      (if (>= i len)
          #t
          (if (fn (vector-ref vec i))
              (loop (+ i 1))
              #f)))))

(define (vector-position test vec #!key (start 0)(end #f))
  (let ((start start)
        (end (or end (vector-length vec))))
    (let loop ((i start))
      (if (< i end)
          (if (test (vector-ref vec i))
              i
              (loop (+ 1 i)))
          #f))))
