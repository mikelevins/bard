;;;; ***********************************************************************
;;;;
;;;; Name:          protocol-comparing.scm
;;;; Project:       Bard
;;;; Purpose:       comparing for equality
;;;; Author:        mikel evins
;;;; Copyright:     2016 by mikel evins
;;;;
;;;; ***********************************************************************

(declare (extended-bindings))
(##include "type-signature-macros.scm")
(##include "protocol-macros.scm")

(define-protocol-function Comparing =
  signatures: (list (signature () 'more (Boolean))))

(define (%bard= . args)
  (if (null? args)
      #t
      (if (null? (cdr args))
          #t
          (and (equal? (car args)(cadr args))
               (apply %bard= (cdr args))))))

(define-primitive-method = () %bard=)



