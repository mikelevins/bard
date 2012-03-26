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
(bard:define-structure-type <frame> bard:frame?)
(bard:define-method bard:type ((thing <frame>))(%object->bard-type thing))

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
(bard:define-method bard:list? ((thing <null>)) #t)
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
(bard:define-method bard:empty? ((thing <null>)) #t)
(bard:define-method bard:empty? ((thing <cons>)) #f)
(bard:define-method bard:empty? ((thing <text>))
  (<= (string-length thing) 0))

(bard:define-function bard:length (ls))
(bard:define-method bard:length ((thing <null>)) 0)
(bard:define-method bard:length ((thing <cons>)) (length thing))
(bard:define-method bard:length ((thing <text>)) (string-length thing))

(bard:define-function bard:first (ls))
(bard:define-method bard:first ((thing <cons>)) (car thing))
(bard:define-method bard:first ((thing <text>)) (string-ref thing 0))

(bard:define-function bard:rest (ls))
(bard:define-method bard:rest ((thing <cons>)) (cdr thing))
(bard:define-method bard:rest ((thing <text>)) (substring thing 1 (string-length thing)))

(bard:define-function bard:last (ls))
(bard:define-method bard:last ((thing <cons>)) (list-ref thing (- (length thing) 1)))
(bard:define-method bard:last ((thing <text>)) (string-ref thing (- (string-length thing) 1)))

(bard:define-function bard:nth (ls n))
(bard:define-method bard:nth ((thing <cons>)(n <fixnum>)) (list-ref thing n))
(bard:define-method bard:nth ((thing <text>)(n <fixnum>)) (string-ref thing n))

(bard:define-function bard:second (ls))
(bard:define-method bard:second ((thing <cons>)) (list-ref thing 1))
(bard:define-method bard:second ((thing <text>)) (string-ref thing 1))

(bard:define-function bard:third (ls))
(bard:define-method bard:third ((thing <cons>)) (list-ref thing 2))
(bard:define-method bard:third ((thing <text>)) (string-ref thing 2))

(bard:define-function bard:tails (ls))
(bard:define-method bard:tails ((thing <null>)) '(()))
(bard:define-method bard:tails ((thing <cons>)) (cons thing (bard:tails (cdr thing))))
(bard:define-method bard:tails ((thing <text>)) 
                    (if (<= (string-length thing) 0)
                        '("")
                        (cons thing (bard:tails (bard:rest thing)))))

(bard:define-function bard:take (n ls))
(bard:define-method bard:take ((n <fixnum>)(thing <null>))(if (zero? n) '() (error "can't take that many elements" n)))
(bard:define-method bard:take ((count <fixnum>)(thing <cons>)) 
                    (let loop ((n count)
                               (ls thing))
                      (if (<= n 0)
                          '()
                          (if (null? ls)
                              (error "can't take that many elements" count)
                              (cons (car ls)
                                    (loop (- n 1) (cdr ls)))))))

(bard:define-method bard:take ((n <fixnum>)(thing <text>))
                    (substring thing 0 n))

(bard:define-function bard:drop (n ls))
(bard:define-method bard:drop ((n <fixnum>)(thing <null>)) (if (zero? n) '() (error "can't drop that many elements" n)))

(bard:define-method bard:drop ((count <fixnum>)(thing <cons>)) 
                    (let loop ((n count)
                               (ls thing))
                      (if (<= n 0)
                          ls
                          (loop (- n 1)(cdr ls)))))

(bard:define-method bard:drop ((n <fixnum>)(thing <text>)) 
                    (substring thing n (string-length thing)))

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
(bard:define-method bard:applicable? ((thing <null>)) #t)
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



