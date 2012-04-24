;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          Compare.scm
;;;; Project:       Bard
;;;; Purpose:       generic comparisons
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************


(define bard:> (%make-function name: '>))
(%function-add-method! bard:> `(,<fixnum> & args) (lambda (x . args) (apply > (cons x args))))
(%function-add-method! bard:> `(,<bignum> & args) (lambda (x . args) (apply > (cons x args))))
(%function-add-method! bard:> `(,<character> & args) (lambda (x . args) (apply char>? (cons x args))))
(%function-add-method! bard:> `(,<string> & args) (lambda (x . args) (apply string>? (cons x args))))

(define bard:< (%make-function name: '<))
(%function-add-method! bard:< `(,<fixnum> & args) (lambda (x . args) (apply < (cons x args))))
(%function-add-method! bard:< `(,<bignum> & args) (lambda (x . args) (apply < (cons x args))))
(%function-add-method! bard:< `(,<character> & args) (lambda (x . args) (apply char<? (cons x args))))
(%function-add-method! bard:< `(,<string> & args) (lambda (x . args) (apply string<? (cons x args))))

(define bard:>= (%make-function name: '>=))
(%function-add-method! bard:>= `(,<fixnum> & args) (lambda (x . args) (apply >= (cons x args))))
(%function-add-method! bard:>= `(,<bignum> & args) (lambda (x . args) (apply >= (cons x args))))
(%function-add-method! bard:>= `(,<character> & args) (lambda (x . args) (apply char>=? (cons x args))))
(%function-add-method! bard:>= `(,<string> & args) (lambda (x . args) (apply string>=? (cons x args))))

(define bard:<= (%make-function name: '<=))
(%function-add-method! bard:<= `(,<fixnum> & args) (lambda (x . args) (apply <= (cons x args))))
(%function-add-method! bard:<= `(,<bignum> & args) (lambda (x . args) (apply <= (cons x args))))
(%function-add-method! bard:<= `(,<character> & args) (lambda (x . args) (apply char<=? (cons x args))))
(%function-add-method! bard:<= `(,<string> & args) (lambda (x . args) (apply string<=? (cons x args))))



