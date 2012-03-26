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

(bard:define-structure-type <frame> bard:frame?)
(bard:define-method bard:type ((thing <frame>))(%object->bard-type thing))

(define (%frame-add-slot fr key val)
  (bard:%make-frame (cons (cons key val) 
                          (bard:%frame-slots fr))))

(define (%frame-add-slots fr . kv-plist)
  (let loop ((kvs kv-plist)
             (slots (bard:%frame-slots fr)))
    (if (null? kvs)
        (bard:%make-frame slots)
        (if (null? (cdr kvs))
            (error "odd number of arguments" kv-plist)
            (let ((k (car kvs))
                  (v (cadr kvs))
                  (more (cddr kvs)))
              (loop more (cons (cons k v) slots)))))))

(define $frame-for-nonframe-value-table (make-table test: equal?))

(define (%ensure-frame-for-nonframe-value! tp . kv-plist)
  (let ((fr (table-ref $frame-for-nonframe-value-table tp #f)))
    (or fr
        (let* ((kvs `(type: ,tp ,@kv-plist))
               (fr (apply bard:frame kvs)))
          (table-set! $frame-for-nonframe-value-table tp fr)
          fr))))

(bard:define-method %frame-for-nonframe-value ((val <null>)) 
                    (%frame-add-slots (%ensure-frame-for-nonframe-value! <null>)
                                      value: val))

(bard:define-method %frame-for-nonframe-value ((val <character>)) 
                    (%frame-add-slots (%ensure-frame-for-nonframe-value! <character>)
                                      value: val))

(bard:define-method %frame-for-nonframe-value ((val <boolean>)) 
                    (%frame-add-slots (%ensure-frame-for-nonframe-value! <boolean>)
                                      value: val))

(bard:define-method %frame-for-nonframe-value ((val <symbol>)) 
                    (%frame-add-slots (%ensure-frame-for-nonframe-value! <symbol>)
                                      value: val
                                      name: (symbol->string val)))

(bard:define-method %frame-for-nonframe-value ((val <keyword>)) 
                    (%frame-add-slots (%ensure-frame-for-nonframe-value! <keyword>)
                                      value: val
                                      name: (keyword->string val)))

(bard:define-method %frame-for-nonframe-value ((val <flonum>)) 
                    (%frame-add-slots (%ensure-frame-for-nonframe-value! <flonum>)
                                      value: val))

(bard:define-method %frame-for-nonframe-value ((val <ratio>)) 
                    (%frame-add-slots (%ensure-frame-for-nonframe-value! <ratio>)
                                      value: val
                                      numerator: (numerator val)
                                      denominator: (denominator val)))

(bard:define-method %frame-for-nonframe-value ((val <fixnum>)) 
                    (%frame-add-slots (%ensure-frame-for-nonframe-value! <fixnum>)
                                      value: val))

(bard:define-method %frame-for-nonframe-value ((val <bignum>)) 
                    (%frame-add-slots (%ensure-frame-for-nonframe-value! <bignum>)
                                      value: val))

(bard:define-method %frame-for-nonframe-value ((val <closure>)) 
                    (%frame-add-slots (%ensure-frame-for-nonframe-value! <closure>)
                                      value: val
                                      closure-type: (cond
                                                     ((%method? val) 'method)
                                                     ((%function? val) 'function)
                                                     (else 'closure))))


(bard:define-method %frame-for-nonframe-value ((val <cons>)) 
                    (%frame-add-slots (%ensure-frame-for-nonframe-value! <cons>)
                                      value: val))

(bard:define-method %frame-for-nonframe-value ((val <text>)) 
                    (%frame-add-slots (%ensure-frame-for-nonframe-value! <text>)
                                      value: val))

;;; API

(bard:define-function bard:keys (frame))
(bard:define-method bard:keys ((frame <undefined>))(bard:keys (%frame-for-nonframe-value frame)))
(bard:define-method bard:keys ((frame <null>)) (bard:keys (%frame-for-nonframe-value frame)))
(bard:define-method bard:keys ((frame <character>)) (bard:keys (%frame-for-nonframe-value frame)))
(bard:define-method bard:keys ((frame <boolean>)) (bard:keys (%frame-for-nonframe-value frame)))
(bard:define-method bard:keys ((frame <symbol>)) (bard:keys (%frame-for-nonframe-value frame)))
(bard:define-method bard:keys ((frame <keyword>)) (bard:keys (%frame-for-nonframe-value frame)))
(bard:define-method bard:keys ((frame <flonum>)) (bard:keys (%frame-for-nonframe-value frame)))
(bard:define-method bard:keys ((frame <ratio>)) (bard:keys (%frame-for-nonframe-value frame)))
(bard:define-method bard:keys ((frame <fixnum>)) (bard:keys (%frame-for-nonframe-value frame)))
(bard:define-method bard:keys ((frame <bignum>)) (bard:keys (%frame-for-nonframe-value frame)))
(bard:define-method bard:keys ((frame <closure>)) (bard:keys (%frame-for-nonframe-value frame)))
(bard:define-method bard:keys ((frame <cons>)) (bard:keys (%frame-for-nonframe-value frame)))
(bard:define-method bard:keys ((frame <text>)) (bard:keys (%frame-for-nonframe-value frame)))
(bard:define-method bard:keys ((frame <frame>)) (map car (bard:%frame-slots frame)))

(define (%frame-lookup-key frame key)
  (let ((entry (assoc key (bard:%frame-slots frame))))
    (if entry (cdr entry) bard:nothing)))

(bard:define-function bard:get (frame key))

(bard:define-method bard:get ((frame <undefined>)(key <undefined>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <undefined>)(key <null>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <undefined>)(key <character>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <undefined>)(key <boolean>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <undefined>)(key <symbol>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <undefined>)(key <keyword>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <undefined>)(key <flonum>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <undefined>)(key <ratio>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <undefined>)(key <fixnum>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <undefined>)(key <bignum>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <undefined>)(key <closure>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <undefined>)(key <cons>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <undefined>)(key <text>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <undefined>)(key <frame>)) (bard:get (%frame-for-nonframe-value frame) key))

(bard:define-method bard:get ((frame <null>)(key <undefined>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <null>)(key <null>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <null>)(key <character>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <null>)(key <boolean>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <null>)(key <symbol>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <null>)(key <keyword>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <null>)(key <flonum>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <null>)(key <ratio>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <null>)(key <fixnum>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <null>)(key <bignum>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <null>)(key <closure>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <null>)(key <cons>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <null>)(key <text>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <null>)(key <frame>)) (bard:get (%frame-for-nonframe-value frame) key))

(bard:define-method bard:get ((frame <character>)(key <undefined>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <character>)(key <null>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <character>)(key <character>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <character>)(key <boolean>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <character>)(key <symbol>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <character>)(key <keyword>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <character>)(key <flonum>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <character>)(key <ratio>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <character>)(key <fixnum>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <character>)(key <bignum>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <character>)(key <closure>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <character>)(key <cons>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <character>)(key <text>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <character>)(key <frame>)) (bard:get (%frame-for-nonframe-value frame) key))

(bard:define-method bard:get ((frame <boolean>)(key <undefined>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <boolean>)(key <null>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <boolean>)(key <character>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <boolean>)(key <boolean>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <boolean>)(key <symbol>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <boolean>)(key <keyword>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <boolean>)(key <flonum>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <boolean>)(key <ratio>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <boolean>)(key <fixnum>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <boolean>)(key <bignum>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <boolean>)(key <closure>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <boolean>)(key <cons>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <boolean>)(key <text>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <boolean>)(key <frame>)) (bard:get (%frame-for-nonframe-value frame) key))

(bard:define-method bard:get ((frame <symbol>)(key <undefined>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <symbol>)(key <null>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <symbol>)(key <character>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <symbol>)(key <boolean>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <symbol>)(key <symbol>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <symbol>)(key <keyword>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <symbol>)(key <flonum>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <symbol>)(key <ratio>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <symbol>)(key <fixnum>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <symbol>)(key <bignum>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <symbol>)(key <closure>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <symbol>)(key <cons>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <symbol>)(key <text>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <symbol>)(key <frame>)) (bard:get (%frame-for-nonframe-value frame) key))

(bard:define-method bard:get ((frame <keyword>)(key <undefined>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <keyword>)(key <null>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <keyword>)(key <character>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <keyword>)(key <boolean>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <keyword>)(key <symbol>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <keyword>)(key <keyword>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <keyword>)(key <flonum>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <keyword>)(key <ratio>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <keyword>)(key <fixnum>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <keyword>)(key <bignum>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <keyword>)(key <closure>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <keyword>)(key <cons>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <keyword>)(key <text>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <keyword>)(key <frame>)) (bard:get (%frame-for-nonframe-value frame) key))

(bard:define-method bard:get ((frame <flonum>)(key <undefined>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <flonum>)(key <null>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <flonum>)(key <character>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <flonum>)(key <boolean>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <flonum>)(key <symbol>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <flonum>)(key <keyword>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <flonum>)(key <flonum>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <flonum>)(key <ratio>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <flonum>)(key <fixnum>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <flonum>)(key <bignum>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <flonum>)(key <closure>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <flonum>)(key <cons>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <flonum>)(key <text>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <flonum>)(key <frame>)) (bard:get (%frame-for-nonframe-value frame) key))

(bard:define-method bard:get ((frame <ratio>)(key <undefined>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <ratio>)(key <null>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <ratio>)(key <character>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <ratio>)(key <boolean>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <ratio>)(key <symbol>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <ratio>)(key <keyword>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <ratio>)(key <flonum>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <ratio>)(key <ratio>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <ratio>)(key <fixnum>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <ratio>)(key <bignum>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <ratio>)(key <closure>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <ratio>)(key <cons>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <ratio>)(key <text>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <ratio>)(key <frame>)) (bard:get (%frame-for-nonframe-value frame) key))

(bard:define-method bard:get ((frame <fixnum>)(key <undefined>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <fixnum>)(key <null>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <fixnum>)(key <character>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <fixnum>)(key <boolean>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <fixnum>)(key <symbol>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <fixnum>)(key <keyword>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <fixnum>)(key <flonum>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <fixnum>)(key <ratio>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <fixnum>)(key <fixnum>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <fixnum>)(key <bignum>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <fixnum>)(key <closure>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <fixnum>)(key <cons>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <fixnum>)(key <text>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <fixnum>)(key <frame>)) (bard:get (%frame-for-nonframe-value frame) key))

(bard:define-method bard:get ((frame <bignum>)(key <undefined>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <bignum>)(key <null>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <bignum>)(key <character>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <bignum>)(key <boolean>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <bignum>)(key <symbol>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <bignum>)(key <keyword>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <bignum>)(key <flonum>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <bignum>)(key <ratio>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <bignum>)(key <fixnum>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <bignum>)(key <bignum>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <bignum>)(key <closure>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <bignum>)(key <cons>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <bignum>)(key <text>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <bignum>)(key <frame>)) (bard:get (%frame-for-nonframe-value frame) key))

(bard:define-method bard:get ((frame <closure>)(key <undefined>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <closure>)(key <null>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <closure>)(key <character>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <closure>)(key <boolean>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <closure>)(key <symbol>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <closure>)(key <keyword>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <closure>)(key <flonum>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <closure>)(key <ratio>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <closure>)(key <fixnum>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <closure>)(key <bignum>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <closure>)(key <closure>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <closure>)(key <cons>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <closure>)(key <text>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <closure>)(key <frame>)) (bard:get (%frame-for-nonframe-value frame) key))

(bard:define-method bard:get ((frame <cons>)(key <undefined>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <cons>)(key <null>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <cons>)(key <character>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <cons>)(key <boolean>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <cons>)(key <symbol>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <cons>)(key <keyword>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <cons>)(key <flonum>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <cons>)(key <ratio>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <cons>)(key <fixnum>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <cons>)(key <bignum>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <cons>)(key <closure>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <cons>)(key <cons>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <cons>)(key <text>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <cons>)(key <frame>)) (bard:get (%frame-for-nonframe-value frame) key))

(bard:define-method bard:get ((frame <text>)(key <undefined>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <text>)(key <null>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <text>)(key <character>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <text>)(key <boolean>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <text>)(key <symbol>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <text>)(key <keyword>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <text>)(key <flonum>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <text>)(key <ratio>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <text>)(key <fixnum>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <text>)(key <bignum>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <text>)(key <closure>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <text>)(key <cons>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <text>)(key <text>)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <text>)(key <frame>)) (bard:get (%frame-for-nonframe-value frame) key))

(bard:define-method bard:get ((frame <frame>)(key <undefined>))(error "can't use undefined as a frame key"))
(bard:define-method bard:get ((frame <frame>)(key <null>))(error "can't use nothing as a frame key"))
(bard:define-method bard:get ((frame <frame>)(key <character>))(%frame-lookup-key frame key))
(bard:define-method bard:get ((frame <frame>)(key <boolean>))(%frame-lookup-key frame key))
(bard:define-method bard:get ((frame <frame>)(key <symbol>))(%frame-lookup-key frame key))
(bard:define-method bard:get ((frame <frame>)(key <keyword>))(%frame-lookup-key frame key))
(bard:define-method bard:get ((frame <frame>)(key <flonum>))(%frame-lookup-key frame key))
(bard:define-method bard:get ((frame <frame>)(key <ratio>))(%frame-lookup-key frame key))
(bard:define-method bard:get ((frame <frame>)(key <fixnum>))(%frame-lookup-key frame key))
(bard:define-method bard:get ((frame <frame>)(key <bignum>))(%frame-lookup-key frame key))
(bard:define-method bard:get ((frame <frame>)(key <closure>))(%frame-lookup-key frame key))
(bard:define-method bard:get ((frame <frame>)(key <cons>))(%frame-lookup-key frame key))
(bard:define-method bard:get ((frame <frame>)(key <text>))(%frame-lookup-key frame key))
(bard:define-method bard:get ((frame <frame>)(key <frame>))(%frame-lookup-key frame key))


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

;;;(define bard:list? (%make-function name: 'list? signature: '(thingg)))

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



