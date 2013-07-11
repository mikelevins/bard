;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          types-primitive-types.scm
;;;; Project:       Bard
;;;; Purpose:       representing primitive types
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

;;; ---------------------------------------------------------------------
;;; built-in schema types
;;; ---------------------------------------------------------------------
;;; runtime values that represent the types of built-in schemas.
;;; these are not the types of schema-instaces; these are the types of
;;; their types. For example, a point may be an instance of
;;; <point>. What is <point> an instance of? The answer is <record>
;;; (or some other of the primitive schemas defined below).
;;; What about these types? For example, what is <primitive-schema>
;;; an instance of? It is an instance of itself: it is itself a
;;; primitive schema.

(define tags:$primitive-schema (%next-bard-type-number))
(define tags:$structure-schema (%next-bard-type-number))
(define tags:$base-schema (%next-bard-type-number))
(define tags:$record-schema (%next-bard-type-number))
(define tags:$tuple-schema (%next-bard-type-number))
(define tags:$union-schema (%next-bard-type-number))
(define tags:$foreign-schema (%next-bard-type-number))

(define <primitive-schema> (make-primitive-schema '<primitive-schema> tags:$primitive-schema))
(%register-primitive-schema! <primitive-schema> tags:$primitive-schema)
(define <structure-schema> (make-primitive-schema '<structure-schema> tags:$structure-schema))
(%register-primitive-schema! <structure-schema> tags:$structure-schema)
(define <base-schema> (make-primitive-schema '<base-schema> tags:$base-schema))
(%register-primitive-schema! <base-schema> tags:$base-schema)
(define <record> (make-primitive-schema '<record> tags:$record-schema))
(%register-primitive-schema! <record> tags:$record-schema)
(define <tuple> (make-primitive-schema '<tuple> tags:$tuple-schema))
(%register-primitive-schema! <tuple> tags:$tuple-schema)
(define <union> (make-primitive-schema '<union> tags:$union-schema))
(%register-primitive-schema! <union> tags:$union-schema)
(define <foreign-schema> (make-primitive-schema '<foreign-schema> tags:$foreign-schema))
(%register-primitive-schema! <foreign-schema> tags:$foreign-schema)

;;; =====================================================================
;;; schema definitions
;;; =====================================================================

;;; ---------------------------------------------------------------------
;;; primitive schemas
;;; ---------------------------------------------------------------------

(define <undefined> (make-primitive-schema '<undefined> tags:$undefined))
(%register-primitive-schema! <undefined> tags:$undefined)
(define <null> (make-primitive-schema '<null> tags:$null))
(%register-primitive-schema! <null> tags:$null)
(define <character> (make-primitive-schema '<character> tags:$character))
(%register-primitive-schema! <character> tags:$character)
(define <boolean> (make-primitive-schema '<boolean> tags:$boolean))
(%register-primitive-schema! <boolean> tags:$boolean)
(define <symbol> (make-primitive-schema '<symbol> tags:$symbol))
(%register-primitive-schema! <symbol> tags:$symbol)
(define <keyword> (make-primitive-schema '<keyword> tags:$keyword))
(%register-primitive-schema! <keyword> tags:$keyword)
(define <flonum> (make-primitive-schema '<flonum> tags:$flonum))
(%register-primitive-schema! <flonum> tags:$flonum)
(define <ratnum> (make-primitive-schema '<ratnum> tags:$ratnum))
(%register-primitive-schema! <ratnum> tags:$ratnum)
(define <fixnum> (make-primitive-schema '<fixnum> tags:$fixnum))
(%register-primitive-schema! <fixnum> tags:$fixnum)
(define <bignum> (make-primitive-schema '<bignum> tags:$bignum))
(%register-primitive-schema! <bignum> tags:$bignum)
(define <primitive-procedure> (make-primitive-schema '<primitive-procedure> tags:$procedure))
(%register-primitive-schema! <primitive-procedure> tags:$procedure)
(define <string> (make-primitive-schema '<string> tags:$string))
(%register-primitive-schema! <string> tags:$string)
(define <pair> (make-primitive-schema '<pair> tags:$pair))
(%register-primitive-schema! <pair> tags:$pair)

(define (%undefined? x)(eqv? x #!unbound))
(define (%defined? x)(not (%undefined? x)))

