;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          singletons.scm
;;;; Project:       bard
;;;; Purpose:       unique Bard values
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

;;; ---------------------------------------------------------------------
;;; eof
;;; ---------------------------------------------------------------------

(define (bard:eof) #!eof)
(define (bard:eof? x) (eqv? x #!eof))

;;; ---------------------------------------------------------------------
;;; undefined
;;; ---------------------------------------------------------------------

(define (bard:undefined) #!unbound)
(define (bard:undefined? x) (eqv? x #!unbound))
(define (bard:defined? x)(not (bard:undefined? x)))

;;; ---------------------------------------------------------------------
;;; nothing
;;; ---------------------------------------------------------------------

(define (bard:nothing) '())
(define (bard:nothing? x) (null? x))
(define (bard:something?? x)(not (bard:nothing? x)))

;;; ---------------------------------------------------------------------
;;; true and false
;;; ---------------------------------------------------------------------

(define (bard:false) #f)
(define (bard:false? x) 
  (or (eqv? x (bard:false))
      (bard:nothing? x)))

(define (bard:true) #t)
(define (bard:true? x) (not (bard:false? x)))

