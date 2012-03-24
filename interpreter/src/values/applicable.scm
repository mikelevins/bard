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

(##include "function-macros.scm")
(##include "type-macros.scm")

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
(bard:define-type <frame> tags:$frame)

;;;(define <frame> (bard:%make-type '<frame> tags:$frame))
;;;(table-set! $bard-type-table tags:$frame <frame>)

;;; ForeignFrame <- <ObjCFrame>

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

;;;(define bard:list? (%make-function name: 'list? signature: '(thing)))

(bard:define-function bard:list? (thing))
(bard:define-method bard:list? ((thing <undefined>)) #f)
(bard:define-method bard:list? ((thing <null>)) #f)
(bard:define-method bard:list? ((thing <character>)) #f)
(bard:define-method bard:list? ((thing <boolean>)) #f)
(bard:define-method bard:list? ((thing <symbol>)) #f)
(bard:define-method bard:list? ((thing <keyword>)) #f)
(bard:define-method bard:list? ((thing <flonum>)) #f)
(bard:define-method bard:list? ((thing <ratio>)) #f)
(bard:define-method bard:list? ((thing <fixnum>)) #f)
(bard:define-method bard:list? ((thing <bignum>)) #f)
(bard:define-method bard:list? ((thing <closure>)) #f)
(bard:define-method bard:list? ((thing <cons>)) #t)
(bard:define-method bard:list? ((thing <text>)) #t)
(bard:define-method bard:list? ((thing <frame>)) #f)


(define bard:list list)

(bard:define-function bard:empty? (ls))
(bard:define-function bard:length (ls))

(bard:define-function bard:first (ls))
(bard:define-function bard:rest (ls))
(bard:define-function bard:last (ls))
(bard:define-function bard:nth (ls n))
(bard:define-function bard:second (ls))
(bard:define-function bard:third (ls))
(bard:define-function bard:tails (ls))

(bard:define-function bard:take (n ls))
(bard:define-function bard:drop (n ls))

(bard:define-function bard:filter (fn ls))
(bard:define-function bard:any? (fn ls))
(bard:define-function bard:every? (fn ls))
(bard:define-function bard:iota (count start step))

(bard:define-function bard:add-first (x ls))
(bard:define-function bard:add-last (ls x))

(bard:define-function bard:append (ls1 ls2))
(bard:define-function bard:reverse (ls1 ls2))

(bard:define-function bard:map (fn ls))
(bard:define-function bard:fold-left (op init ls))
(bard:define-function bard:fold-right (op init ls))
(bard:define-function bard:reduce (op init ls))

(bard:define-function bard:member? (k ls test))
(bard:define-function bard:assoc (k ls test))

;;; ForeignList <- <NSArray>
;;; ForeignText <- <NSString>

;;; ---------------------------------------------------------------------
;;; Applicable protocol
;;; ---------------------------------------------------------------------

(bard:define-function bard:applicable? (thing))
(bard:define-method bard:applicable? ((thing <undefined>)) #f)
(bard:define-method bard:applicable? ((thing <null>)) #f)
(bard:define-method bard:applicable? ((thing <character>)) #f)
(bard:define-method bard:applicable? ((thing <boolean>)) #f)
(bard:define-method bard:applicable? ((thing <symbol>)) #f)
(bard:define-method bard:applicable? ((thing <keyword>)) #f)
(bard:define-method bard:applicable? ((thing <flonum>)) #f)
(bard:define-method bard:applicable? ((thing <ratio>)) #f)
(bard:define-method bard:applicable? ((thing <fixnum>)) #f)
(bard:define-method bard:applicable? ((thing <bignum>)) #f)
(bard:define-method bard:applicable? ((thing <closure>)) #t)
(bard:define-method bard:applicable? ((thing <cons>)) #f)
(bard:define-method bard:applicable? ((thing <text>)) #f)
(bard:define-method bard:applicable? ((thing <frame>)) #t)

(bard:define-function bard:apply (app arg))

