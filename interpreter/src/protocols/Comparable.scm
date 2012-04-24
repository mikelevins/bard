;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          Comparable.scm
;;;; Project:       Bard
;;;; Purpose:       generic comparisons
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(##include "../values/type-macros.scm")

;;; ---------------------------------------------------------------------
;;; The Protocol
;;; ---------------------------------------------------------------------

(%define-protocol Comparable)

;;; comparable?
;;; ---------------------------------------------------------------------

(define bard:comparable? (%make-function name: 'comparable?))
(%function-add-method! bard:comparable? `(,Anything ,Anything)(lambda (x y)(bard:false)))
(%function-add-method! bard:comparable? `(,<fixnum> ,<fixnum>)(lambda (x y)(bard:true)))
(%function-add-method! bard:comparable? `(,<fixnum> ,<bignum>)(lambda (x y)(bard:true)))
(%function-add-method! bard:comparable? `(,<bignum> ,<fixnum>)(lambda (x y)(bard:true)))
(%function-add-method! bard:comparable? `(,<character> ,<character>)(lambda (x y)(bard:true)))
(%function-add-method! bard:comparable? `(,<string> ,<string>)(lambda (x y)(bard:true)))

;;; =
;;; ---------------------------------------------------------------------

(define bard:= (%make-function name: '=))
(%function-add-method! bard:= `(,Anything ,Anything)(lambda (x y)(equal? x y)))

;;; >
;;; ---------------------------------------------------------------------

(define bard:> (%make-function name: '>))
(%function-add-method! bard:> `(,<fixnum> & args) (lambda (x . args) (apply > (cons x args))))
(%function-add-method! bard:> `(,<bignum> & args) (lambda (x . args) (apply > (cons x args))))
(%function-add-method! bard:> `(,<character> & args) (lambda (x . args) (apply char>? (cons x args))))
(%function-add-method! bard:> `(,<string> & args) (lambda (x . args) (apply string>? (cons x args))))

;;; <
;;; ---------------------------------------------------------------------

(define bard:< (%make-function name: '<))
(%function-add-method! bard:< `(,<fixnum> & args) (lambda (x . args) (apply < (cons x args))))
(%function-add-method! bard:< `(,<bignum> & args) (lambda (x . args) (apply < (cons x args))))
(%function-add-method! bard:< `(,<character> & args) (lambda (x . args) (apply char<? (cons x args))))
(%function-add-method! bard:< `(,<string> & args) (lambda (x . args) (apply string<? (cons x args))))

;;; >=
;;; ---------------------------------------------------------------------

(define bard:>= (%make-function name: '>=))
(%function-add-method! bard:>= `(,<fixnum> & args) (lambda (x . args) (apply >= (cons x args))))
(%function-add-method! bard:>= `(,<bignum> & args) (lambda (x . args) (apply >= (cons x args))))
(%function-add-method! bard:>= `(,<character> & args) (lambda (x . args) (apply char>=? (cons x args))))
(%function-add-method! bard:>= `(,<string> & args) (lambda (x . args) (apply string>=? (cons x args))))

;;; <=
;;; ---------------------------------------------------------------------

(define bard:<= (%make-function name: '<=))
(%function-add-method! bard:<= `(,<fixnum> & args) (lambda (x . args) (apply <= (cons x args))))
(%function-add-method! bard:<= `(,<bignum> & args) (lambda (x . args) (apply <= (cons x args))))
(%function-add-method! bard:<= `(,<character> & args) (lambda (x . args) (apply char<=? (cons x args))))
(%function-add-method! bard:<= `(,<string> & args) (lambda (x . args) (apply string<=? (cons x args))))



