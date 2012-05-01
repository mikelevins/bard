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
(##include "../values/function-macros.scm")

;;; ---------------------------------------------------------------------
;;; The Protocol
;;; ---------------------------------------------------------------------

(%define-protocol Frame)

;;; frame?
;;; ---------------------------------------------------------------------

(define bard:frame? (%make-function name: 'frame?))

(%function-add-method! bard:frame? `(,Anything) (%method (thing) false))
(%function-add-method! bard:frame? `(,<null>) (%method (thing) true))
(%function-add-method! bard:frame? `(,<cons>) (%method (thing) true))
(%function-add-method! bard:frame? `(,<string>) (%method (thing) true))
(%function-add-method! bard:frame? `(,<frame>) (%method (thing) true))

;;; contains-key?
;;; ---------------------------------------------------------------------

(define bard:contains-key? (%make-function name: 'contains-key?))


(define %bard-contains-key?
  (%primitive-method (fr thing & args)
                     (let ((test (if (null? args)
                                     bard:=
                                     (car args)))
                           (keys (%keys fr)))
                       (if (any? (lambda (k) (%apply test (list thing k))) keys)
                           (%true)
                           (%false)))))

(define %bard-list-contains-key? 
  (%primitive-method (ls thing & args)
                     (let* ((test (if (null? args)
                                      bard:=
                                      (car args)))
                            (items (%as-list ls))
                            (keys (range 0 (length items))))
                       (if (any? (lambda (k) (%apply test (list thing k))) keys)
                           (%true)
                           (%false)))))

(%function-add-method! bard:contains-key? `(,<null> ,Anything & args) (%method (thing) false))
(%function-add-method! bard:contains-key? `(,<cons> ,Anything & args) %bard-list-contains-key?)
(%function-add-method! bard:contains-key? `(,<string> ,Anything & args) %bard-list-contains-key?)
(%function-add-method! bard:contains-key? `(,<frame> ,Anything & args) %bard-contains-key?)

;;; contains-value?
;;; ---------------------------------------------------------------------

(define bard:contains-value? (%make-function name: 'contains-value?))

(define %bard-contains-value? 
  (%primitive-method (fr thing & args)
                     (let ((test (if (null? args)
                                     bard:=
                                     (car args)))
                           (vals (if (%frame? fr)
                                     (%vals fr)
                                     (%as-list fr))))
                       (if (any? (lambda (v) (%apply test (list thing v))) vals)
                           (%true)
                           (%false)))))

(%function-add-method! bard:contains-value? `(,<null> ,Anything & args) (%method (thing) false))
(%function-add-method! bard:contains-value? `(,<cons> ,Anything & args) %bard-contains-value?)
(%function-add-method! bard:contains-value? `(,<string> ,Anything & args) %bard-contains-value?)
(%function-add-method! bard:contains-value? `(,<frame> ,Anything & args) %bard-contains-value?)

;;; get
;;; ---------------------------------------------------------------------

(define bard:get (%make-function name: 'get))

(define %bard-get 
  (%primitive-method (fr thing & args)
                     (let ((default (if (null? args)
                                        (%nothing)
                                        (cadr args))))
                       (%frame-get fr thing default))))

(define %bard-get-from-list 
  (%primitive-method (ls i & args)
                     (let ((default (if (null? args)
                                        (%nothing)
                                        (cadr args)))
                           (ls (%as-list ls)))
                       (if (< -1 i (length ls))
                           (list-ref ls i)
                           default))))

(%function-add-method! bard:get `(,<null> ,Anything & args) (lambda (fr thing . args)(%nothing)))
(%function-add-method! bard:get `(,<cons> ,<fixnum> & args) %bard-get-from-list)
(%function-add-method! bard:get `(,<cons> ,<bignum> & args) %bard-get-from-list)
(%function-add-method! bard:get `(,<string> ,<fixnum> & args) %bard-get-from-list)
(%function-add-method! bard:get `(,<string> ,<bignum> & args) %bard-get-from-list)
(%function-add-method! bard:get `(,<frame> ,Anything & args) %bard-get)

;;; keys
;;; ---------------------------------------------------------------------

(define bard:keys (%make-function name: 'keys))

(%function-add-method! bard:keys `(,<null>) (%primitive-method (fr)(%nothing)))
(%function-add-method! bard:keys `(,<cons>) (%primitive-method (ls)(range 0 (length ls))))
(%function-add-method! bard:keys `(,<string>) (%primitive-method (str)(range 0 (string-length str))))
(%function-add-method! bard:keys `(,<frame>) (%primitive-method (fr)(%keys fr)))

;;; merge
;;; ---------------------------------------------------------------------

(define bard:merge (%make-function name: 'merge))

(define %bard-merge 
  (%primitive-method (ls1 ls2 & args)
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
                               (loop (cdr items) new-result)))))))

(define %bard-merge-frames 
  (%primitive-method (fr1 fr2 & args)
                     (%frame-merge fr1 fr2)))

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

(define %bard-put 
  (%primitive-method (fr k v)
                     (%frame-put fr k v)))



(define %bard-put-in-null
  (%primitive-method (ls k v)
                     (if (zero? k)
                         (list v)
                         (error "index out of range"))))

(define %bard-put-in-list 
  (%primitive-method (ls k v)
                     (let* ((tp (%object->bard-type ls))
                            (items (%as-list ls))
                            (new-items (if (= k (length items))
                                           (reverse (cons v (reverse items)))
                                           (if (< -1 k (length items))
                                               (append (take k items)
                                                       (cons v
                                                             (drop (+ k 1) items)))
                                               (error "Index out of range")))))
                       (%to-type tp new-items))))

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

(%function-add-method! bard:vals `(,<null>) (%method (x) x))
(%function-add-method! bard:vals `(,<cons>) (%method (x) x))
(%function-add-method! bard:vals `(,<string>) (%primitive-method (x) (%as-list x)))
(%function-add-method! bard:vals `(,<frame>) (%primitive-method (fr) (%vals fr)))
