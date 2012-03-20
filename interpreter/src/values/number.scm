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

(define (bard:integer? x)
  (or (##fixnum? x)
      (##bignum? x)))

(define (bard:float? x)
  (##flonum? x))

(define (bard:ratio? x)
  (##ratnum? x))

(define (bard:number? x) 
  (or (bard:integer? x)
      (bard:float? x)
      (bard:ratio? x)))

