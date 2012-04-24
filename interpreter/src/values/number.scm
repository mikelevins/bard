;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          number.scm
;;;; Project:       Bard
;;;; Purpose:       number values
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

;;;---------------------------------------------------------------------
;;; API
;;;---------------------------------------------------------------------

(define %fixnum? ##fixnum?)
(define %fixnum? ##bignum?)

(define (%integer? x)
  (or (##fixnum? x)
      (##bignum? x)))

(define (%float? x)
  (##flonum? x))

(define (%ratio? x)
  (##ratnum? x))

(define (%number? x) 
  (or (%integer? x)
      (%float? x)
      (%ratio? x)))

