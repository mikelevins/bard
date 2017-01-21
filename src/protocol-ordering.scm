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

(declare (extended-bindings))
(##include "type-signature-macros.scm")
(##include "protocol-macros.scm")

(define-protocol-function Ordering <
  signatures: (list (signature (Orderable Orderable) #f (Boolean))))

(define-primitive-method < (<fixnum> <fixnum>) <)
(define-primitive-method < (<fixnum> <bignum>) <)
(define-primitive-method < (<fixnum> <flonum>) <)
(define-primitive-method < (<fixnum> <ratnum>) <)

(define-primitive-method < (<bignum> <bignum>) <)
(define-primitive-method < (<bignum> <fixnum>) <)
(define-primitive-method < (<bignum> <flonum>) <)
(define-primitive-method < (<bignum> <ratnum>) <)

(define-primitive-method < (<flonum> <flonum>) <)
(define-primitive-method < (<flonum> <fixnum>) <)
(define-primitive-method < (<flonum> <bignum>) <)
(define-primitive-method < (<flonum> <ratnum>) <)

(define-primitive-method < (<ratnum> <ratnum>) <)
(define-primitive-method < (<ratnum> <fixnum>) <)
(define-primitive-method < (<ratnum> <bignum>) <)
(define-primitive-method < (<ratnum> <flonum>) <)

(define-primitive-method < (<character> <character>) char<?)
(define-primitive-method < (<string> <string>) string<?)

(define-protocol-function Ordering >
  signatures: (list (signature (Orderable Orderable) #f (Boolean))))

(define-primitive-method > (<fixnum> <fixnum>) >)
(define-primitive-method > (<fixnum> <bignum>) >)
(define-primitive-method > (<fixnum> <flonum>) >)
(define-primitive-method > (<fixnum> <ratnum>) >)

(define-primitive-method > (<bignum> <bignum>) >)
(define-primitive-method > (<bignum> <fixnum>) >)
(define-primitive-method > (<bignum> <flonum>) >)
(define-primitive-method > (<bignum> <ratnum>) >)

(define-primitive-method > (<flonum> <flonum>) >)
(define-primitive-method > (<flonum> <fixnum>) >)
(define-primitive-method > (<flonum> <bignum>) >)
(define-primitive-method > (<flonum> <ratnum>) >)

(define-primitive-method > (<ratnum> <ratnum>) >)
(define-primitive-method > (<ratnum> <fixnum>) >)
(define-primitive-method > (<ratnum> <bignum>) >)
(define-primitive-method > (<ratnum> <flonum>) >)

(define-primitive-method > (<character> <character>) char>?)
(define-primitive-method > (<string> <string>) string>?)

(define-protocol-function Ordering <=
  signatures: (list (signature (Orderable Orderable) #f (Boolean))))

(define-primitive-method <= (<fixnum> <fixnum>) <=)
(define-primitive-method <= (<fixnum> <bignum>) <=)
(define-primitive-method <= (<fixnum> <flonum>) <=)
(define-primitive-method <= (<fixnum> <ratnum>) <=)

(define-primitive-method <= (<bignum> <bignum>) <=)
(define-primitive-method <= (<bignum> <fixnum>) <=)
(define-primitive-method <= (<bignum> <flonum>) <=)
(define-primitive-method <= (<bignum> <ratnum>) <=)

(define-primitive-method <= (<flonum> <flonum>) <=)
(define-primitive-method <= (<flonum> <fixnum>) <=)
(define-primitive-method <= (<flonum> <bignum>) <=)
(define-primitive-method <= (<flonum> <ratnum>) <=)

(define-primitive-method <= (<ratnum> <ratnum>) <=)
(define-primitive-method <= (<ratnum> <fixnum>) <=)
(define-primitive-method <= (<ratnum> <bignum>) <=)
(define-primitive-method <= (<ratnum> <flonum>) <=)

(define-primitive-method <= (<character> <character>) char<=?)
(define-primitive-method <= (<string> <string>) string<=?)

(define-protocol-function Ordering >=
  signatures: (list (signature (Orderable Orderable) #f (Boolean))))

(define-primitive-method >= (<fixnum> <fixnum>) >=)
(define-primitive-method >= (<fixnum> <bignum>) >=)
(define-primitive-method >= (<fixnum> <flonum>) >=)
(define-primitive-method >= (<fixnum> <ratnum>) >=)

(define-primitive-method >= (<bignum> <bignum>) >=)
(define-primitive-method >= (<bignum> <fixnum>) >=)
(define-primitive-method >= (<bignum> <flonum>) >=)
(define-primitive-method >= (<bignum> <ratnum>) >=)

(define-primitive-method >= (<flonum> <flonum>) >=)
(define-primitive-method >= (<flonum> <fixnum>) >=)
(define-primitive-method >= (<flonum> <bignum>) >=)
(define-primitive-method >= (<flonum> <ratnum>) >=)

(define-primitive-method >= (<ratnum> <ratnum>) >=)
(define-primitive-method >= (<ratnum> <fixnum>) >=)
(define-primitive-method >= (<ratnum> <bignum>) >=)
(define-primitive-method >= (<ratnum> <flonum>) >=)

(define-primitive-method >= (<character> <character>) char>=?)
(define-primitive-method >= (<string> <string>) string>=?)



