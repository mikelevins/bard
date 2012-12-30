;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          schemas-base.scm
;;;; Project:       Bard
;;;; Purpose:       definitions of base schemas
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(define <undefined> (%define-base-schema '<undefined> tags:$undefined))
(define <null> (%define-base-schema '<null> tags:$null))
(define <character> (%define-base-schema '<character> tags:$character))
(define <boolean> (%define-base-schema '<boolean> tags:$boolean))
(define <symbol> (%define-base-schema '<symbol> tags:$symbol))
(define <keyword> (%define-base-schema '<keyword> tags:$keyword))
(define <flonum> (%define-base-schema '<flonum> tags:$flonum))
(define <ratnum> (%define-base-schema '<ratnum> tags:$ratnum))
(define <fixnum> (%define-base-schema '<fixnum> tags:$fixnum))
(define <bignum> (%define-base-schema '<bignum> tags:$bignum))
(define <primitive-procedure> (%define-base-schema '<primitive-procedure> tags:$procedure))
(define <string> (%define-base-schema '<string> tags:$string))
(define <pair> (%define-base-schema '<list> tags:$pair))
(define <foreign-value> (%define-base-schema '<foreign-value> tags:$foreign-value))

