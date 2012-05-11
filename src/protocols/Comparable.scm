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
(##include "../values/function-macros.scm")

;;; ---------------------------------------------------------------------
;;; The Protocol
;;; ---------------------------------------------------------------------

(%define-protocol Comparable)

;;; comparable?
;;; ---------------------------------------------------------------------

(define bard:comparable? (%make-function name: 'comparable?))
(%function-add-method! bard:comparable? `(,Anything ,Anything)(%method (x y) false))
(%function-add-method! bard:comparable? `(,<fixnum> ,<fixnum>)(%method (x y) true))
(%function-add-method! bard:comparable? `(,<fixnum> ,<bignum>)(%method (x y) true))
(%function-add-method! bard:comparable? `(,<bignum> ,<fixnum>)(%method (x y) true))
(%function-add-method! bard:comparable? `(,<bignum> ,<bignum>)(%method (x y) true))
(%function-add-method! bard:comparable? `(,<character> ,<character>)(%method (x y) true))
(%function-add-method! bard:comparable? `(,<string> ,<string>)(%method (x y) true))

;;; =
;;; ---------------------------------------------------------------------

(define bard:= (%make-function name: '=))
(%function-add-method! bard:= `(,Anything ,Anything)(%primitive-method (x y)(equal? x y)))

;;; >
;;; ---------------------------------------------------------------------

(define bard:> (%make-function name: '>))
(%function-add-method! bard:> `(,<fixnum> & args) (%primitive-method (x & args) (apply > (cons x args))))
(%function-add-method! bard:> `(,<bignum> & args) (%primitive-method (x & args) (apply > (cons x args))))
(%function-add-method! bard:> `(,<character> & args) (%primitive-method (x & args) (apply char>? (cons x args))))
(%function-add-method! bard:> `(,<string> & args) (%primitive-method (x & args) (apply string>? (cons x args))))

;;; <
;;; ---------------------------------------------------------------------

(define bard:< (%make-function name: '<))
(%function-add-method! bard:< `(,<fixnum> & args) (%primitive-method (x & args) (apply < (cons x args))))
(%function-add-method! bard:< `(,<bignum> & args) (%primitive-method (x & args) (apply < (cons x args))))
(%function-add-method! bard:< `(,<character> & args) (%primitive-method (x & args) (apply char<? (cons x args))))
(%function-add-method! bard:< `(,<string> & args) (%primitive-method (x & args) (apply string<? (cons x args))))

;;; >=
;;; ---------------------------------------------------------------------

(define bard:>= (%make-function name: '>=))
(%function-add-method! bard:>= `(,<fixnum> & args) (%primitive-method (x & args) (apply >= (cons x args))))
(%function-add-method! bard:>= `(,<bignum> & args) (%primitive-method (x & args) (apply >= (cons x args))))
(%function-add-method! bard:>= `(,<character> & args) (%primitive-method (x & args) (apply char>=? (cons x args))))
(%function-add-method! bard:>= `(,<string> & args) (%primitive-method (x & args) (apply string>=? (cons x args))))

;;; <=
;;; ---------------------------------------------------------------------

(define bard:<= (%make-function name: '<=))
(%function-add-method! bard:<= `(,<fixnum> & args) (%primitive-method (x & args) (apply <= (cons x args))))
(%function-add-method! bard:<= `(,<bignum> & args) (%primitive-method (x & args) (apply <= (cons x args))))
(%function-add-method! bard:<= `(,<character> & args) (%primitive-method (x & args) (apply char<=? (cons x args))))
(%function-add-method! bard:<= `(,<string> & args) (%primitive-method (x & args) (apply string<=? (cons x args))))

