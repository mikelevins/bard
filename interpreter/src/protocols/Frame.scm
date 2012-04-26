;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          Frame.scm
;;;; Project:       Bard
;;;; Purpose:       implementation of the Frame protocol
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(##include "../values/type-macros.scm")

;;; ---------------------------------------------------------------------
;;; The Protocol
;;; ---------------------------------------------------------------------

(%define-protocol Frame)

;;; frame?
;;; ---------------------------------------------------------------------

(define bard:frame? (%make-function name: 'frame?))

(%function-add-method! bard:frame? `(,Anything) (lambda (thing) (%false)))
(%function-add-method! bard:frame? `(,<null>) (lambda (thing) (%true)))
(%function-add-method! bard:frame? `(,<cons>) (lambda (thing) (%true)))
(%function-add-method! bard:frame? `(,<string>) (lambda (thing) (%true)))
(%function-add-method! bard:frame? `(,<frame>) (lambda (thing) (%true)))

;;; contains-key?
;;; ---------------------------------------------------------------------

(define bard:contains-key? (%make-function name: 'contains-key?))

(define (%bard-contains-key? fr thing . args)
  (let ((test (if (null? args)
                  bard:=
                  (car args)))
        (keys (%keys fr)))
    (if (any? (lambda (k) (%apply test (list thing k))) keys)
        (%true)
        (%false))))

(define (%bard-list-contains-key? ls thing . args)
  (let* ((test (if (null? args)
                   bard:=
                   (car args)))
         (items (%as-list ls))
         (keys (range 0 (length items))))
    (if (any? (lambda (k) (%apply test (list thing k))) keys)
        (%true)
        (%false))))

(%function-add-method! bard:contains-key? `(,<null> ,Anything & args) %bard-list-contains-key?)
(%function-add-method! bard:contains-key? `(,<cons> ,Anything & args) %bard-list-contains-key?)
(%function-add-method! bard:contains-key? `(,<string> ,Anything & args) %bard-list-contains-key?)
(%function-add-method! bard:contains-key? `(,<frame> ,Anything & args) %bard-contains-key?)

;;; contains-value?
;;; ---------------------------------------------------------------------

(define bard:contains-value? (%make-function name: 'contains-value?))

(define (%bard-contains-value? fr thing . args)
  (let ((test (if (null? args)
                  bard:=
                  (car args)))
        (vals (if (%frame? fr)
                  (%vals fr)
                  (%as-list fr))))
    (if (any? (lambda (v) (%apply test (list thing v))) vals)
        (%true)
        (%false))))

(%function-add-method! bard:contains-value? `(,<null> ,Anything & args) (%false))
(%function-add-method! bard:contains-value? `(,<cons> ,Anything & args) %bard-contains-value?)
(%function-add-method! bard:contains-value? `(,<string> ,Anything & args) %bard-contains-value?)
(%function-add-method! bard:contains-value? `(,<frame> ,Anything & args) %bard-contains-value?)

;;; get
;;; ---------------------------------------------------------------------

(define bard:get (%make-function name: 'get))

(define (%bard-get fr thing . args)
  (let ((default (if (null? args)
                     (%nothing)
                     (cadr args))))
    (%frame-get fr thing default)))

(define (%bard-get-from-list ls i . args)
  (let ((default (if (null? args)
                     (%nothing)
                     (cadr args)))
        (ls (%as-list ls)))
    (if (< 0 (+ i 1) (length ls))
        (list-ref ls i)
        default)))

(%function-add-method! bard:get `(,<null> ,Anything & args) (lambda (fr thing . args)(%nothing)))
(%function-add-method! bard:get `(,<cons> ,<fixnum> & args) %bard-get-from-list)
(%function-add-method! bard:get `(,<cons> ,<bignum> & args) %bard-get-from-list)
(%function-add-method! bard:get `(,<string> ,<fixnum> & args) %bard-get-from-list)
(%function-add-method! bard:get `(,<string> ,<bignum> & args) %bard-get-from-list)
(%function-add-method! bard:get `(,<frame> ,Anything & args) %bard-get)

;;; keys
;;; ---------------------------------------------------------------------

(define bard:keys (%make-function name: 'keys))

(%function-add-method! bard:keys `(,<null>) (lambda (fr)(%nothing)))
(%function-add-method! bard:keys `(,<cons>) (lambda (ls)(%range 0 (length ls))))
(%function-add-method! bard:keys `(,<string>) (lambda (str)(%range 0 (string-length ls))))
(%function-add-method! bard:keys `(,<frame>) (lambda (fr)(%keys fr)))

;;; merge
;;; ---------------------------------------------------------------------

(define bard:merge (%make-function name: 'merge))

(define (%bard-merge ls1 ls2 . args)
  (let* ((tp (%object->bard-type ls1))
         (test (if (null? args)
                   bard:=
                   (car args)))
         (member? (lambda (x s)(any? (lambda (i)(%apply test (list i x))) s))))
    (let loop ((items (append (%as-list ls1)(%as-list ls2)))
               (result '()))
      (if (null? items)
          (%to-type tp (reverse result))
          (let* ((item (car items))
                 (new-result (if (member? item result)
                                 result
                                 (cons item result))))
            (loop (cdr items) new-result))))))

(define (%bard-merge-frames fr1 fr2 . args)
  (%frame-merge fr1 fr2))

(%function-add-method! bard:merge `(,<null> ,<null> & args) %bard-merge)
(%function-add-method! bard:merge `(,<null> ,<cons> & args) %bard-merge)
(%function-add-method! bard:merge `(,<null> ,<string> & args) %bard-merge)
(%function-add-method! bard:merge `(,<null> ,<frame> & args) %bard-merge)
(%function-add-method! bard:merge `(,<cons> ,<null> & args) %bard-merge)
(%function-add-method! bard:merge `(,<cons> ,<cons> & args) %bard-merge)
(%function-add-method! bard:merge `(,<cons> ,<string> & args) %bard-merge)
(%function-add-method! bard:merge `(,<cons> ,<frame> & args) %bard-merge)
(%function-add-method! bard:merge `(,<string> ,<null> & args) %bard-merge)
(%function-add-method! bard:merge `(,<string> ,<cons> & args) %bard-merge)
(%function-add-method! bard:merge `(,<string> ,<string> & args) %bard-merge)
(%function-add-method! bard:merge `(,<string> ,<frame> & args) %bard-merge)
(%function-add-method! bard:merge `(,<frame> ,<null> & args) %bard-merge)
(%function-add-method! bard:merge `(,<frame> ,<cons> & args) %bard-merge)
(%function-add-method! bard:merge `(,<frame> ,<string> & args) %bard-merge)
(%function-add-method! bard:merge `(,<frame> ,<frame> & args) %bard-merge-frames)


;;; put
;;; ---------------------------------------------------------------------

(define bard:put (%make-function name: 'put))

(define (%bard-put fr k v)(%frame-put fr k v))

(define (%bard-put-in-null ls k v)
  (if (zero? k)
      (list v)
      (error "index out of range")))

(define (%bard-put-in-list ls k v)
  (let ((tp (%object->bard-type ls))
        (items (%as-list ls))
        (new-items (if (= k (length items))
                       (reverse (cons v (reverse items)))
                       (if (< 0 (+ k 1) (length items))
                           (append (take k items)
                                   (cons v
                                         (drop (+ k 1) items)))
                           (error "Index out of range")))))
    (%to-type tp new-items)))

(%function-add-method! bard:put `(,<null> ,Anything ,Anything) %bard-put-in-null)
(%function-add-method! bard:put `(,<null> ,<fixnum> ,Anything) %bard-put-in-null)
(%function-add-method! bard:put `(,<cons> ,<fixnum> ,Anything) %bard-put-in-list)
(%function-add-method! bard:put `(,<cons> ,<bignum> ,Anything) %bard-put-in-list)
(%function-add-method! bard:put `(,<string> ,<fixnum> ,Anything) %bard-put-in-list)
(%function-add-method! bard:put `(,<string> ,<bignum> ,Anything) %bard-put-in-list)
(%function-add-method! bard:put `(,<frame> ,Anything ,Anything) %bard-put)

;;; vals
;;; ---------------------------------------------------------------------

(define bard:vals (%make-function name: 'vals))

(define (%bard-vals fr)(%vals fr))

(%function-add-method! bard:vals `(,<null>) (lambda (x) x))
(%function-add-method! bard:vals `(,<cons>) (lambda (x) x))
(%function-add-method! bard:vals `(,<string>) (lambda (x) (%as-list x)))
(%function-add-method! bard:vals `(,<frame>) %bard-vals)

