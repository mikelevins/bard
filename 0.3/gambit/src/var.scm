;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          var.scm
;;;; Project:       Bard
;;;; Purpose:       representation of variables
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(define (%var-val var)
  (car var))

(define (%set-var-val! var val)
  (set-car! var val))

(define (%var-name var)
  (cadr var))

(define (%var-setter var)
  (cddr var))

(define (%make-var val name #!key (mutable #f))
  (let ((var (cons val (cons name #f))))
    (if mutable
        (let ((setter (lambda (v)(set-car! var v))))
          (set-cdr! (cdr var) setter)))
    var))

