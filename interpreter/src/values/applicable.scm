;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          applicable.scm
;;;; Project:       Bard
;;;; Purpose:       applicable types (including lists and text)
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************


;;; ---------------------------------------------------------------------
;;; cons
;;; ---------------------------------------------------------------------

(define bard:cons? pair?)

;;; ---------------------------------------------------------------------
;;; text
;;; ---------------------------------------------------------------------

(define bard:text? string?)

;;; ---------------------------------------------------------------------
;;; frame
;;; ---------------------------------------------------------------------

(define-type bard:frame
  id: 08C172EF-8046-4ADC-BC26-86E4244C9F5A
  constructor: bard:%make-frame
  (slots bard:%frame-slots))

(define (bard:make-frame key-val-plist)
  (let loop ((kvs key-val-plist)
             (slots '()))
    (if (null? kvs)
        (bard:%make-frame slots)
        (if (null? (cdr kvs))
            (error "malformed inputs to make-frame" key-val-plist)
            (let ((k (car kvs))
                  (v (cadr kvs))
                  (more (cddr kvs)))
              (loop more (cons (cons k v) slots)))))))

(define (bard:frame . kvs)
  (bard:make-frame kvs))

(define tags:$frame 4)
(define <frame> (bard:%make-type '<frame> tags:$frame))
(table-set! $bard-type-table tags:$frame <frame>)

;;; ---------------------------------------------------------------------
;;; function
;;; ---------------------------------------------------------------------

(define bard:function? %function?)

;;; ---------------------------------------------------------------------
;;; method
;;; ---------------------------------------------------------------------

(define bard:method? %method?)

;;; ---------------------------------------------------------------------
;;; List protocol
;;; ---------------------------------------------------------------------

(define bard:list? (%make-function name: 'list? signature: '(thing)))
(%add-method! bard:list? (%make-method name: 'list? signature: `((thing ,<undefined>)) method-function: (lambda (x) #f)))
(%add-method! bard:list? (%make-method name: 'list? signature: `((thing ,<null>)) method-function: (lambda (x) #f)))
(%add-method! bard:list? (%make-method name: 'list? signature: `((thing ,<character>)) method-function: (lambda (x) #f)))
(%add-method! bard:list? (%make-method name: 'list? signature: `((thing ,<boolean>)) method-function: (lambda (x) #f)))
(%add-method! bard:list? (%make-method name: 'list? signature: `((thing ,<symbol>)) method-function: (lambda (x) #f)))
(%add-method! bard:list? (%make-method name: 'list? signature: `((thing ,<keyword>)) method-function: (lambda (x) #f)))
(%add-method! bard:list? (%make-method name: 'list? signature: `((thing ,<flonum>)) method-function: (lambda (x) #f)))
(%add-method! bard:list? (%make-method name: 'list? signature: `((thing ,<ratio>)) method-function: (lambda (x) #f)))
(%add-method! bard:list? (%make-method name: 'list? signature: `((thing ,<fixnum>)) method-function: (lambda (x) #f)))
(%add-method! bard:list? (%make-method name: 'list? signature: `((thing ,<bignum>)) method-function: (lambda (x) #f)))
(%add-method! bard:list? (%make-method name: 'list? signature: `((thing ,<closure>)) method-function: (lambda (x) #f)))
(%add-method! bard:list? (%make-method name: 'list? signature: `((thing ,<cons>)) method-function: (lambda (x) #t)))
(%add-method! bard:list? (%make-method name: 'list? signature: `((thing ,<text>)) method-function: (lambda (x) #t)))
(%add-method! bard:list? (%make-method name: 'list? signature: `((thing ,<frame>)) method-function: (lambda (x) #f)))

;;; (bard:list? 1)
;;; (bard:list? '(1 2))

(define bard:list list)

;;; ---------------------------------------------------------------------
;;; Applicable protocol
;;; ---------------------------------------------------------------------

(define bard:applicable? (%make-function name: 'applicable? signature: '(thing)))
(%add-method! bard:applicable? (%make-method name: 'applicable? signature: `((thing ,<undefined>)) method-function: (lambda (x) #f)))
(%add-method! bard:applicable? (%make-method name: 'applicable? signature: `((thing ,<null>)) method-function: (lambda (x) #f)))
(%add-method! bard:applicable? (%make-method name: 'applicable? signature: `((thing ,<character>)) method-function: (lambda (x) #f)))
(%add-method! bard:applicable? (%make-method name: 'applicable? signature: `((thing ,<boolean>)) method-function: (lambda (x) #f)))
(%add-method! bard:applicable? (%make-method name: 'applicable? signature: `((thing ,<symbol>)) method-function: (lambda (x) #f)))
(%add-method! bard:applicable? (%make-method name: 'applicable? signature: `((thing ,<keyword>)) method-function: (lambda (x) #f)))
(%add-method! bard:applicable? (%make-method name: 'applicable? signature: `((thing ,<flonum>)) method-function: (lambda (x) #f)))
(%add-method! bard:applicable? (%make-method name: 'applicable? signature: `((thing ,<ratio>)) method-function: (lambda (x) #f)))
(%add-method! bard:applicable? (%make-method name: 'applicable? signature: `((thing ,<fixnum>)) method-function: (lambda (x) #f)))
(%add-method! bard:applicable? (%make-method name: 'applicable? signature: `((thing ,<bignum>)) method-function: (lambda (x) #f)))
(%add-method! bard:applicable? (%make-method name: 'applicable? signature: `((thing ,<closure>)) method-function: (lambda (x) #f)))
(%add-method! bard:applicable? (%make-method name: 'applicable? signature: `((thing ,<cons>)) method-function: (lambda (x) #f)))
(%add-method! bard:applicable? (%make-method name: 'applicable? signature: `((thing ,<text>)) method-function: (lambda (x) #f)))
(%add-method! bard:applicable? (%make-method name: 'applicable? signature: `((thing ,<frame>)) method-function: (lambda (x) #t)))

