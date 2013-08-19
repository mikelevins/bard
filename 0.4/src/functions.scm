;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          functions.scm
;;;; Project:       Bard
;;;; Purpose:       representation of vm functions
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

(declare (standard-bindings)
         (extended-bindings)
         (inline)
         (inline-primitives))

;;; ----------------------------------------------------------------------
;;; fns
;;; ----------------------------------------------------------------------

(define-structure fn required-parameters rest-parameter environment body)

