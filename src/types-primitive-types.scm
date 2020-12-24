;;;; ***********************************************************************
;;;;
;;;; Name:          types-primitive-types.scm
;;;; Project:       Bard
;;;; Purpose:       representing primitive types
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

;;; ---------------------------------------------------------------------
;;; built-in bard-structure types
;;; ---------------------------------------------------------------------
;;; runtime values that represent the types of built-in
;;; bard-structures.  these are not the types of
;;; bard-structure-instances; these are the types of their types. For
;;; example, a point may be an instance of <point>. What is <point> an
;;; instance of?  What about that type? For example, what is
;;; <primitive-structure> an instance of? It is an instance of itself:
;;; it is itself a primitive bard-structure.

(define tags:$primitive-structure (%next-bard-type-number))
(define tags:$structure-bard-structure (%next-bard-type-number))
(define tags:$base-bard-structure (%next-bard-type-number))
(define tags:$foreign-bard-structure (%next-bard-type-number))

(define <primitive-structure> (make-primitive-structure '<primitive-structure> tags:$primitive-structure))
(%register-primitive-structure! <primitive-structure> tags:$primitive-structure)
(define <structure-structure> (make-primitive-structure '<structure-structure> tags:$structure-bard-structure))
(%register-primitive-structure! <structure-structure> tags:$structure-bard-structure)
(define <base-structure> (make-primitive-structure '<base-structure> tags:$base-bard-structure))
(%register-primitive-structure! <base-structure> tags:$base-bard-structure)
(define <foreign-structure> (make-primitive-structure '<foreign-structure> tags:$foreign-bard-structure))
(%register-primitive-structure! <foreign-structure> tags:$foreign-bard-structure)

;;; =====================================================================
;;; bard-structure definitions
;;; =====================================================================

;;; ---------------------------------------------------------------------
;;; primitive bard-structures
;;; ---------------------------------------------------------------------

(define <undefined> (make-primitive-structure '<undefined> tags:$undefined))
(%register-primitive-structure! <undefined> tags:$undefined)
(define <null> (make-primitive-structure '<null> tags:$null))
(%register-primitive-structure! <null> tags:$null)
(define <character> (make-primitive-structure '<character> tags:$character))
(%register-primitive-structure! <character> tags:$character)
(define <boolean> (make-primitive-structure '<boolean> tags:$boolean))
(%register-primitive-structure! <boolean> tags:$boolean)
(define <symbol> (make-primitive-structure '<symbol> tags:$symbol))
(%register-primitive-structure! <symbol> tags:$symbol)
(define <keyword> (make-primitive-structure '<keyword> tags:$keyword))
(%register-primitive-structure! <keyword> tags:$keyword)
(define <flonum> (make-primitive-structure '<flonum> tags:$flonum))
(%register-primitive-structure! <flonum> tags:$flonum)
(define <ratnum> (make-primitive-structure '<ratnum> tags:$ratnum))
(%register-primitive-structure! <ratnum> tags:$ratnum)
(define <fixnum> (make-primitive-structure '<fixnum> tags:$fixnum))
(%register-primitive-structure! <fixnum> tags:$fixnum)
(define <bignum> (make-primitive-structure '<bignum> tags:$bignum))
(%register-primitive-structure! <bignum> tags:$bignum)
(define <primitive-procedure> (make-primitive-structure '<primitive-procedure> tags:$procedure))
(%register-primitive-structure! <primitive-procedure> tags:$procedure)
(define <string> (make-primitive-structure '<string> tags:$string))
(%register-primitive-structure! <string> tags:$string)
(define <vector> (make-primitive-structure '<vector> tags:$vector))
(%register-primitive-structure! <vector> tags:$vector)
(define <pair> (make-primitive-structure '<pair> tags:$pair))
(%register-primitive-structure! <pair> tags:$pair)

(define (%undefined? x)(eqv? x #!unbound))
(define (%defined? x)(not (%undefined? x)))

