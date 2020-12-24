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
;;; runtime values that represent the types of built-in bard-structures.
;;; these are not the types of bard-structure-instances; these are the types of
;;; their types. For example, a point may be an instance of
;;; <point>. What is <point> an instance of? The answer is <record>
;;; (or some other of the primitive bard-structures defined below).
;;; What about these types? For example, what is <primitive-structure>
;;; an instance of? It is an instance of itself: it is itself a
;;; primitive bard-structure.

(define tags:$primitive-bard-structure (%next-bard-type-number))
(define tags:$structure-bard-structure (%next-bard-type-number))
(define tags:$base-bard-structure (%next-bard-type-number))
(define tags:$record-bard-structure (%next-bard-type-number))
(define tags:$tuple-bard-structure (%next-bard-type-number))
(define tags:$union-bard-structure (%next-bard-type-number))
(define tags:$foreign-bard-structure (%next-bard-type-number))

(define <primitive-structure> (make-primitive-bard-structure '<primitive-structure> tags:$primitive-bard-structure))
(%register-primitive-bard-structure! <primitive-structure> tags:$primitive-bard-structure)
(define <structure-structure> (make-primitive-bard-structure '<structure-structure> tags:$structure-bard-structure))
(%register-primitive-bard-structure! <structure-structure> tags:$structure-bard-structure)
(define <base-structure> (make-primitive-bard-structure '<base-structure> tags:$base-bard-structure))
(%register-primitive-bard-structure! <base-structure> tags:$base-bard-structure)
(define <record> (make-primitive-bard-structure '<record> tags:$record-bard-structure))
(%register-primitive-bard-structure! <record> tags:$record-bard-structure)
(define <tuple> (make-primitive-bard-structure '<tuple> tags:$tuple-bard-structure))
(%register-primitive-bard-structure! <tuple> tags:$tuple-bard-structure)
(define <union> (make-primitive-bard-structure '<union> tags:$union-bard-structure))
(%register-primitive-bard-structure! <union> tags:$union-bard-structure)
(define <foreign-structure> (make-primitive-bard-structure '<foreign-structure> tags:$foreign-bard-structure))
(%register-primitive-bard-structure! <foreign-structure> tags:$foreign-bard-structure)

;;; =====================================================================
;;; bard-structure definitions
;;; =====================================================================

;;; ---------------------------------------------------------------------
;;; primitive bard-structures
;;; ---------------------------------------------------------------------

(define <undefined> (make-primitive-bard-structure '<undefined> tags:$undefined))
(%register-primitive-bard-structure! <undefined> tags:$undefined)
(define <null> (make-primitive-bard-structure '<null> tags:$null))
(%register-primitive-bard-structure! <null> tags:$null)
(define <character> (make-primitive-bard-structure '<character> tags:$character))
(%register-primitive-bard-structure! <character> tags:$character)
(define <boolean> (make-primitive-bard-structure '<boolean> tags:$boolean))
(%register-primitive-bard-structure! <boolean> tags:$boolean)
(define <symbol> (make-primitive-bard-structure '<symbol> tags:$symbol))
(%register-primitive-bard-structure! <symbol> tags:$symbol)
(define <keyword> (make-primitive-bard-structure '<keyword> tags:$keyword))
(%register-primitive-bard-structure! <keyword> tags:$keyword)
(define <flonum> (make-primitive-bard-structure '<flonum> tags:$flonum))
(%register-primitive-bard-structure! <flonum> tags:$flonum)
(define <ratnum> (make-primitive-bard-structure '<ratnum> tags:$ratnum))
(%register-primitive-bard-structure! <ratnum> tags:$ratnum)
(define <fixnum> (make-primitive-bard-structure '<fixnum> tags:$fixnum))
(%register-primitive-bard-structure! <fixnum> tags:$fixnum)
(define <bignum> (make-primitive-bard-structure '<bignum> tags:$bignum))
(%register-primitive-bard-structure! <bignum> tags:$bignum)
(define <primitive-procedure> (make-primitive-bard-structure '<primitive-procedure> tags:$procedure))
(%register-primitive-bard-structure! <primitive-procedure> tags:$procedure)
(define <string> (make-primitive-bard-structure '<string> tags:$string))
(%register-primitive-bard-structure! <string> tags:$string)
(define <vector> (make-primitive-bard-structure '<vector> tags:$vector))
(%register-primitive-bard-structure! <vector> tags:$vector)
(define <pair> (make-primitive-bard-structure '<pair> tags:$pair))
(%register-primitive-bard-structure! <pair> tags:$pair)

(define (%undefined? x)(eqv? x #!unbound))
(define (%defined? x)(not (%undefined? x)))

