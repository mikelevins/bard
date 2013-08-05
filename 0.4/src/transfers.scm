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
;;; continuations
;;; ----------------------------------------------------------------------

(define-structure continuation stack environment destination)

(define (vmstate-continue! state cc)
  (let ((stack (continuation-stack cc))
        (env (continuation-environment cc))
        (dest (continuation-destination cc)))
    (if stack (vmstate-stack-set! state (continuation-stack cc)))
    (if env (vmstate-env-set! state (continuation-environment cc)))
    (if dest (vmstate-pc-set! state (continuation-destination cc))))
  state)
