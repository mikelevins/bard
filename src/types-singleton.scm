;;;; ***********************************************************************
;;;;
;;;; Name:          types-primitive.scm
;;;; Project:       Bard
;;;; Purpose:       bard-structure <primitive>
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

;;; ----------------------------------------------------------------------
;;; <singleton>
;;; ----------------------------------------------------------------------

(define tags:$bard-singleton (%next-bard-type-number))
(define <singleton> (make-base-bard-structure '<singleton> tags:$bard-singleton))

;;; singleton registry

(define +singletons+ (make-table test: equal?))

(define (%existing-singleton val)
  (table-ref +singletons+ val #f))

(define (%register-singleton! val sing)
  (table-set! +singletons+ val sing))

;;; constructor

(define (%singleton val)
  (or (%existing-singleton val)
      (let ((sing (make-singleton-instance <singleton> val)))
        (%register-singleton! val sing)
        sing)))

;;; accessors

(define singleton-value singleton-instance-value)
(define %singleton? singleton-instance?)

