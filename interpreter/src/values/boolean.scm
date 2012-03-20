;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          boolean.scm
;;;; Project:       Bard
;;;; Purpose:       boolean values
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

;;;---------------------------------------------------------------------
;;; API
;;;---------------------------------------------------------------------

(define (bard:false) #f)

(define (bard:false? x) 
  (or (eqv? x (bard:false))
      (bard:nothing? x)))

(define (bard:true) #t)
(define (bard:true? x) (not (bard:false? x)))

(define (bard:boolean? x)
  (or (eqv? x (bard:false))
      (eqv? x (bard:true))))

