;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          opmacros.scm
;;;; Project:       Bard
;;;; Purpose:       macro support for vm operations
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************


(define-macro (pushv! v) `(push! $vals ,v))
(define-macro (popv!) `(pop! $vals))
(define-macro (opcode i) `(vector-ref ,i 0))

(define-macro (arg1) `(vector-ref (vector-ref $code $pc) 1))
(define-macro (arg2) `(vector-ref (vector-ref $code $pc) 1))
(define-macro (setpc!) `(set! $pc (arg1)))
(define-macro (incpc!) `(set! $pc (+ $pc 1)))
(define-macro (lref) `(vector-ref (vector-ref $env (arg1)) (arg2)))
(define-macro (lset!) `(vector-set! (vector-ref $env (arg1)) (arg2) (popv!)))
(define-macro (mref) `(vector-ref (vector-ref $module (arg1)) (arg2)))
(define-macro (mset!) `(vector-set! (vector-ref $module (arg1)) (arg2) (popv!)))




