;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          compiler.scm
;;;; Project:       bard
;;;; Purpose:       compile abstract syntax to bardvm code
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

(define (bard:%emit op . params)
  (list (cons op params)))

(define (bard:%self-evaluating? exp)
  (bard:syntax-atom? exp))

(define (bard:%gen-CONST exp)
  (bard:%emit op_CONST (bard:%syntax-atom-value exp)))

(define (bard:compile exp env)
  (if (bard:%self-evaluating? exp)
      (bard:%gen-CONST exp)
      (error "compilation of non-self-evaluating expressions not yet implemented")))
