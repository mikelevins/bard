;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          constants.scm
;;;; Project:       Bard
;;;; Purpose:       virtual machine constants
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

(declare (standard-bindings)
         (extended-bindings)
         (inline)
         (inline-primitives))

;;; ----------------------------------------------------------------------
;;; 
;;; ----------------------------------------------------------------------

(define +absent+ (string->symbol "+absent+"))
(define +bard-program-version+ (vector 0 4 0))
