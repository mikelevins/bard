;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          transfers.scm
;;;; Project:       Bard
;;;; Purpose:       representation and implementation of transfers of control
;;;;                (i. e. function returns, continuations, and conditions)
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

(declare (standard-bindings)
         (extended-bindings)
         (inline)
         (inline-primitives))

;;; ----------------------------------------------------------------------
;;; return records
;;; ----------------------------------------------------------------------

(define-structure return pc fn stack)

(define (vmstate-return! state ret vals)
  (vmstate-pc-set! (return-pc ret))
  (vmstate-stack-set! (return-stack ret))
  (vmstate-fn-set! state (return-fn ret))
  (vmstate-pushvals! state vals)
  state)

;;; ----------------------------------------------------------------------
;;; continuations
;;; ----------------------------------------------------------------------

(define-structure continuation pc stack env)

(define (vmstate-continue! state cc)
  (vmstate-pc-set! state (continuation-pc cc))
  (vmstate-stack-set! state (continuation-stack cc))
  (vmstate-env-set! state (continuation-env cc))
  state)
