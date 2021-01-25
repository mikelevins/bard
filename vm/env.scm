;;;; ***********************************************************************
;;;;
;;;; Name:          env.scm
;;;; Project:       Bard
;;;; Purpose:       representation of lexical environments
;;;; Author:        mikel evins
;;;; Copyright:     2021 by mikel evins
;;;;
;;;; ***********************************************************************

(define-structure fn code env name args)


(define (env-get env var) #f)
(define (env-set! env var val) #f)

