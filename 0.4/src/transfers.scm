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
;;; function return records
;;; ----------------------------------------------------------------------

(define-structure return fn pc stack env)

;;; ----------------------------------------------------------------------
;;; continuations
;;; ----------------------------------------------------------------------

(define-structure continuation stack)


