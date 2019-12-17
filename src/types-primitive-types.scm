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
;;; built-in struct types
;;; ---------------------------------------------------------------------
;;; runtime values that represent the types of built-in structs.
;;; these are not the types of struct-instaces; these are the types of
;;; their types. For example, a point may be an instance of
;;; <point>. What is <point> an instance of? The answer is <record>
;;; (or some other of the primitive structs defined below).
;;; What about these types? For example, what is <primitive-struct>
;;; an instance of? It is an instance of itself: it is itself a
;;; primitive struct.

(define tags:$primitive-struct (%next-bard-type-number))
(define tags:$structure-struct (%next-bard-type-number))
(define tags:$base-struct (%next-bard-type-number))
(define tags:$record-struct (%next-bard-type-number))
(define tags:$tuple-struct (%next-bard-type-number))
(define tags:$union-struct (%next-bard-type-number))
(define tags:$foreign-struct (%next-bard-type-number))

(define <primitive-struct> (make-primitive-struct '<primitive-struct> tags:$primitive-struct))
(%register-primitive-struct! <primitive-struct> tags:$primitive-struct)
(define <structure-struct> (make-primitive-struct '<structure-struct> tags:$structure-struct))
(%register-primitive-struct! <structure-struct> tags:$structure-struct)
(define <base-struct> (make-primitive-struct '<base-struct> tags:$base-struct))
(%register-primitive-struct! <base-struct> tags:$base-struct)
(define <record> (make-primitive-struct '<record> tags:$record-struct))
(%register-primitive-struct! <record> tags:$record-struct)
(define <tuple> (make-primitive-struct '<tuple> tags:$tuple-struct))
(%register-primitive-struct! <tuple> tags:$tuple-struct)
(define <union> (make-primitive-struct '<union> tags:$union-struct))
(%register-primitive-struct! <union> tags:$union-struct)
(define <foreign-struct> (make-primitive-struct '<foreign-struct> tags:$foreign-struct))
(%register-primitive-struct! <foreign-struct> tags:$foreign-struct)

;;; =====================================================================
;;; struct definitions
;;; =====================================================================

;;; ---------------------------------------------------------------------
;;; primitive structs
;;; ---------------------------------------------------------------------

(define <undefined> (make-primitive-struct '<undefined> tags:$undefined))
(%register-primitive-struct! <undefined> tags:$undefined)
(define <null> (make-primitive-struct '<null> tags:$null))
(%register-primitive-struct! <null> tags:$null)
(define <character> (make-primitive-struct '<character> tags:$character))
(%register-primitive-struct! <character> tags:$character)
(define <boolean> (make-primitive-struct '<boolean> tags:$boolean))
(%register-primitive-struct! <boolean> tags:$boolean)
(define <symbol> (make-primitive-struct '<symbol> tags:$symbol))
(%register-primitive-struct! <symbol> tags:$symbol)
(define <keyword> (make-primitive-struct '<keyword> tags:$keyword))
(%register-primitive-struct! <keyword> tags:$keyword)
(define <flonum> (make-primitive-struct '<flonum> tags:$flonum))
(%register-primitive-struct! <flonum> tags:$flonum)
(define <ratnum> (make-primitive-struct '<ratnum> tags:$ratnum))
(%register-primitive-struct! <ratnum> tags:$ratnum)
(define <fixnum> (make-primitive-struct '<fixnum> tags:$fixnum))
(%register-primitive-struct! <fixnum> tags:$fixnum)
(define <bignum> (make-primitive-struct '<bignum> tags:$bignum))
(%register-primitive-struct! <bignum> tags:$bignum)
(define <primitive-procedure> (make-primitive-struct '<primitive-procedure> tags:$procedure))
(%register-primitive-struct! <primitive-procedure> tags:$procedure)
(define <string> (make-primitive-struct '<string> tags:$string))
(%register-primitive-struct! <string> tags:$string)
(define <vector> (make-primitive-struct '<vector> tags:$vector))
(%register-primitive-struct! <vector> tags:$vector)
(define <pair> (make-primitive-struct '<pair> tags:$pair))
(%register-primitive-struct! <pair> tags:$pair)

(define (%undefined? x)(eqv? x #!unbound))
(define (%defined? x)(not (%undefined? x)))

