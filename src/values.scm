;;;; ***********************************************************************
;;;;
;;;; Name:          values.scm
;;;; Project:       Bard
;;;; Purpose:       built-in primitive values
;;;; Author:        mikel evins
;;;; Copyright:     2016 by mikel evins
;;;;
;;;; ***********************************************************************

(declare (extended-bindings))

;;; ---------------------------------------------------------------------
;;; nothing
;;; ---------------------------------------------------------------------

(define (bard:nothing) '())

;;; ---------------------------------------------------------------------
;;; true and false
;;; ---------------------------------------------------------------------

(define (bard:false) #f)
(define (bard:true) #t)

(define (bard:false? thing)
  (or (eqv? thing (bard:false))
      (eqv? thing (bard:nothing))))

(define (bard:true? thing)
  (not (bard:false? thing)))
