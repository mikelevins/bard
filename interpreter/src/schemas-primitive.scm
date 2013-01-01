;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          schemas-primitive.scm
;;;; Project:       Bard
;;;; Purpose:       definitions of primitive schemas
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(define <undefined> (make-primitive-schema '<undefined> tags:$undefined))
(%register-tagged-schema! <undefined> tags:$undefined)
(define <null> (make-primitive-schema '<null> tags:$null))
(%register-tagged-schema! <null> tags:$null)
(define <character> (make-primitive-schema '<character> tags:$character))
(%register-tagged-schema! <character> tags:$character)
(define <boolean> (make-primitive-schema '<boolean> tags:$boolean))
(%register-tagged-schema! <boolean> tags:$boolean)
(define <symbol> (make-primitive-schema '<symbol> tags:$symbol))
(%register-tagged-schema! <symbol> tags:$symbol)
(define <keyword> (make-primitive-schema '<keyword> tags:$keyword))
(%register-tagged-schema! <keyword> tags:$keyword)
(define <flonum> (make-primitive-schema '<flonum> tags:$flonum))
(%register-tagged-schema! <flonum> tags:$flonum)
(define <ratnum> (make-primitive-schema '<ratnum> tags:$ratnum))
(%register-tagged-schema! <ratnum> tags:$ratnum)
(define <fixnum> (make-primitive-schema '<fixnum> tags:$fixnum))
(%register-tagged-schema! <fixnum> tags:$fixnum)
(define <bignum> (make-primitive-schema '<bignum> tags:$bignum))
(%register-tagged-schema! <bignum> tags:$bignum)
(define <primitive-procedure> (make-primitive-schema '<primitive-procedure> tags:$procedure))
(%register-tagged-schema! <primitive-procedure> tags:$procedure)
(define <string> (make-primitive-schema '<string> tags:$string))
(%register-tagged-schema! <string> tags:$string)
(define <pair> (make-primitive-schema '<pair> tags:$pair))
(%register-tagged-schema! <pair> tags:$pair)


