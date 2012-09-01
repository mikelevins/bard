;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          compile.scm
;;;; Project:       Bard
;;;; Purpose:       compiling application forms
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(define (%compile-fun-call op params) #f)

(define (%compile-application-form expr env)
  (let ((op (%compile (car expr) env))
        (params (map (lambda (p)(%compile p env))
                     (cdr env))))
    (if (%prim? op)
        (%compile-prim-call op params)
        (%compile-fun-call op params))))
