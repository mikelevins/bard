;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          compile.scm
;;;; Project:       Bard
;;;; Purpose:       Bard compiler
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************



(define (%compile expr env)
  (cond
   ((%self-evaluating? expr) (%compile-self-evaluating expr env))
   ((%variable-ref? expr) (%compile-variable-ref expr env))
   ((%special-form? expr) (%compile-special-form expr env))
   ((%macro-form? expr) (%compile (%macroexpand expr) env))
   (else (%compile-application-form expr env))))
