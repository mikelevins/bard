;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          protocol-ordering.scm
;;;; Project:       Bard
;;;; Purpose:       arranging values by magnitude
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(define bard:< (make-function debug-name: '<
                              input-types: `(,Orderable ,Orderable)
                              restarg: #f
                              output-types: `(,Boolean)))

(%add-primitive-method! bard:< (list <fixnum> <fixnum>) < debug-name: '<)
(%add-primitive-method! bard:< (list <fixnum> <bignum>) < debug-name: '<)
(%add-primitive-method! bard:< (list <fixnum> <flonum>) < debug-name: '<)
(%add-primitive-method! bard:< (list <fixnum> <ratnum>) < debug-name: '<)

(%add-primitive-method! bard:< (list <bignum> <bignum>) < debug-name: '<)
(%add-primitive-method! bard:< (list <bignum> <fixnum>) < debug-name: '<)
(%add-primitive-method! bard:< (list <bignum> <flonum>) < debug-name: '<)
(%add-primitive-method! bard:< (list <bignum> <ratnum>) < debug-name: '<)

(%add-primitive-method! bard:< (list <flonum> <flonum>) < debug-name: '<)
(%add-primitive-method! bard:< (list <flonum> <fixnum>) < debug-name: '<)
(%add-primitive-method! bard:< (list <flonum> <bignum>) < debug-name: '<)
(%add-primitive-method! bard:< (list <flonum> <ratnum>) < debug-name: '<)

(%add-primitive-method! bard:< (list <ratnum> <ratnum>) < debug-name: '<)
(%add-primitive-method! bard:< (list <ratnum> <fixnum>) < debug-name: '<)
(%add-primitive-method! bard:< (list <ratnum> <bignum>) < debug-name: '<)
(%add-primitive-method! bard:< (list <ratnum> <flonum>) < debug-name: '<)

(%add-primitive-method! bard:< (list <string> <string>) string<? debug-name: '<)

(define bard:> (make-function debug-name: '>
                              input-types: `(,Orderable ,Orderable)
                              restarg: #f
                              output-types: `(,Boolean)))

(%add-primitive-method! bard:> (list <fixnum> <fixnum>) > debug-name: '>)
(%add-primitive-method! bard:> (list <fixnum> <bignum>) > debug-name: '>)
(%add-primitive-method! bard:> (list <fixnum> <flonum>) > debug-name: '>)
(%add-primitive-method! bard:> (list <fixnum> <ratnum>) > debug-name: '>)

(%add-primitive-method! bard:> (list <bignum> <bignum>) > debug-name: '>)
(%add-primitive-method! bard:> (list <bignum> <fixnum>) > debug-name: '>)
(%add-primitive-method! bard:> (list <bignum> <flonum>) > debug-name: '>)
(%add-primitive-method! bard:> (list <bignum> <ratnum>) > debug-name: '>)

(%add-primitive-method! bard:> (list <flonum> <flonum>) > debug-name: '>)
(%add-primitive-method! bard:> (list <flonum> <fixnum>) > debug-name: '>)
(%add-primitive-method! bard:> (list <flonum> <bignum>) > debug-name: '>)
(%add-primitive-method! bard:> (list <flonum> <ratnum>) > debug-name: '>)

(%add-primitive-method! bard:> (list <ratnum> <ratnum>) > debug-name: '>)
(%add-primitive-method! bard:> (list <ratnum> <fixnum>) > debug-name: '>)
(%add-primitive-method! bard:> (list <ratnum> <bignum>) > debug-name: '>)
(%add-primitive-method! bard:> (list <ratnum> <flonum>) > debug-name: '>)

(%add-primitive-method! bard:> (list <string> <string>) string>? debug-name: '>)

(define bard:<= (make-function debug-name: '<=
                               input-types: `(,Orderable ,Orderable)
                               restarg: #f
                               output-types: `(,Boolean)))

(%add-primitive-method! bard:<= (list <fixnum> <fixnum>) <= debug-name: '<=)
(%add-primitive-method! bard:<= (list <fixnum> <bignum>) <= debug-name: '<=)
(%add-primitive-method! bard:<= (list <fixnum> <flonum>) <= debug-name: '<=)
(%add-primitive-method! bard:<= (list <fixnum> <ratnum>) <= debug-name: '<=)

(%add-primitive-method! bard:<= (list <bignum> <bignum>) <= debug-name: '<=)
(%add-primitive-method! bard:<= (list <bignum> <fixnum>) <= debug-name: '<=)
(%add-primitive-method! bard:<= (list <bignum> <flonum>) <= debug-name: '<=)
(%add-primitive-method! bard:<= (list <bignum> <ratnum>) <= debug-name: '<=)

(%add-primitive-method! bard:<= (list <flonum> <flonum>) <= debug-name: '<=)
(%add-primitive-method! bard:<= (list <flonum> <fixnum>) <= debug-name: '<=)
(%add-primitive-method! bard:<= (list <flonum> <bignum>) <= debug-name: '<=)
(%add-primitive-method! bard:<= (list <flonum> <ratnum>) <= debug-name: '<=)

(%add-primitive-method! bard:<= (list <ratnum> <ratnum>) <= debug-name: '<=)
(%add-primitive-method! bard:<= (list <ratnum> <fixnum>) <= debug-name: '<=)
(%add-primitive-method! bard:<= (list <ratnum> <bignum>) <= debug-name: '<=)
(%add-primitive-method! bard:<= (list <ratnum> <flonum>) <= debug-name: '<=)

(%add-primitive-method! bard:<= (list <string> <string>) string<=? debug-name: '<=)

(define bard:>= (make-function debug-name: '>=
                               input-types: `(,Orderable ,Orderable)
                               restarg: #f
                               output-types: `(,Boolean)))

(%add-primitive-method! bard:>= (list <fixnum> <fixnum>) >= debug-name: '>=)
(%add-primitive-method! bard:>= (list <fixnum> <bignum>) >= debug-name: '>=)
(%add-primitive-method! bard:>= (list <fixnum> <flonum>) >= debug-name: '>=)
(%add-primitive-method! bard:>= (list <fixnum> <ratnum>) >= debug-name: '>=)

(%add-primitive-method! bard:>= (list <bignum> <bignum>) >= debug-name: '>=)
(%add-primitive-method! bard:>= (list <bignum> <fixnum>) >= debug-name: '>=)
(%add-primitive-method! bard:>= (list <bignum> <flonum>) >= debug-name: '>=)
(%add-primitive-method! bard:>= (list <bignum> <ratnum>) >= debug-name: '>=)

(%add-primitive-method! bard:>= (list <flonum> <flonum>) >= debug-name: '>=)
(%add-primitive-method! bard:>= (list <flonum> <fixnum>) >= debug-name: '>=)
(%add-primitive-method! bard:>= (list <flonum> <bignum>) >= debug-name: '>=)
(%add-primitive-method! bard:>= (list <flonum> <ratnum>) >= debug-name: '>=)

(%add-primitive-method! bard:>= (list <ratnum> <ratnum>) >= debug-name: '>=)
(%add-primitive-method! bard:>= (list <ratnum> <fixnum>) >= debug-name: '>=)
(%add-primitive-method! bard:>= (list <ratnum> <bignum>) >= debug-name: '>=)
(%add-primitive-method! bard:>= (list <ratnum> <flonum>) >= debug-name: '>=)

(%add-primitive-method! bard:>= (list <string> <string>) string>=? debug-name: '>=)



