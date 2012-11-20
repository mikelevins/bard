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

;;; a var is represented as: (val . (name . setter-function))
;;; if setter-function is #f, then the var is read-only

(define (%make-var name val #!key (mutable #f))
  (let* ((meta (cons name #f))
         (var (cons val meta)))
    (if mutable
        (set-cdr! meta (lambda (x)(set-car! var x))))
    var))

(define (%var-val v)(car v))
(define (%var-name v)(cadr v))
(define (%var-setter v)(cddr v))
(define (%var-mutable? v)(and (cddr v) #t))


