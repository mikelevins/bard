;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          values.scm
;;;; Project:       bard
;;;; Purpose:       representations of base value types
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
(define (bard:something? x)(not (bard:nothing? x)))

;;; ---------------------------------------------------------------------
;;; true and false
;;; ---------------------------------------------------------------------

(define (bard:false) #f)
(define (bard:false? x) 
  (or (eqv? x (bard:false))
      (bard:nothing? x)))

(define (bard:true) #t)
(define (bard:true? x) (not (bard:false? x)))

;;; ---------------------------------------------------------------------
;;; sequences
;;; ---------------------------------------------------------------------

(define bard:empty-sequence #f)
(define bard:empty-sequence? #f)
(let ((empty-seq (make-wt-tree number-wt-type)))
  (set! bard:empty-sequence (lambda () empty-seq))
  (set! bard:empty-sequence? (lambda (x) (eq? x empty-seq))))

