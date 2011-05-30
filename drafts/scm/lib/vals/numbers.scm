;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          numbers.scm
;;;; Project:       bard
;;;; Purpose:       Bard numeric values
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

;;; ---------------------------------------------------------------------
;;; numbers
;;; ---------------------------------------------------------------------

(define (bard:integer x)
  (or (and (integer? x)
           x)
      (error "Invalid value for integer" x)))

(define (bard:integer? x)
  (integer? x))

(define (bard:float x)
  (or (and (flonum? x)
           x)
      (error "Invalid value for float" x)))

(define (bard:float? x)
  (flonum? x))

(define (bard:ratio x)
  (or (and (##ratnum? x)
           x)
      (error "Invalid value for ratio" x)))

(define (bard:ratio? x)
  (##ratnum? x))

(define (bard:number x)
  (cond
   ((integer? x)(bard:integer x))
   ((float? x)(bard:float x))
   ((ratio? x)(bard:ratio x))
   (else "Invalid value for number" x)))
