;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          prims.scm
;;;; Project:       Bard
;;;; Purpose:       primitive procedures, defined in Scheme, but bound to
;;;;                Bard variables in the initial environment
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(define (prim:read . args) 
  (let ((in (if (null? args)
                (current-input-port)
                (car args))))
    (bard:read in)))

(define (prim:eval expr . args) 
  (let ((env (if (null? args)
                 (%top-level-environment)
                 (car args))))
    (%eval expr env)))

(define (prim:apply fn args . rest) 
  (let ((env (if (null? rest)
                 (%top-level-environment)
                 (car rest))))
    (%apply fn args env)))

(define (prim:print thing . args)
  (let ((out (if (null? args)
                 (current-output-port)
                 (car args))))
    (bard:print thing out)))

;;; Number

(define (prim:+) )
(define (prim:-) )
(define (prim:*) )
(define (prim:/) )
