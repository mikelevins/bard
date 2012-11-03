;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          compile.scm
;;;; Project:       Bard
;;;; Purpose:       bard compiler
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************



(define (%get-compiler-for stype)
  (case stype
    ((syntax:app) %compile-app)
    ((syntax:character) %compile-self-evaluating)
    ((syntax:empty-app) %compile-self-evaluating)
    ((syntax:empty-list) %compile-self-evaluating)
    ((syntax:empty-string) %compile-self-evaluating)
    ((syntax:empty-table) %compile-self-evaluating)
    ((syntax:false) %compile-self-evaluating)
    ((syntax:keyword) %compile-self-evaluating)
    ((syntax:lambda-list) %compile-method-lambda-list)
    ((syntax:list) %compile-self-evaluating)
    ((syntax:method) %compile-method)
    ((syntax:nothing) %compile-self-evaluating)
    ((syntax:number) %compile-self-evaluating)
    ((syntax:self-evaluating) %compile-self-evaluating)
    ((syntax:series) %compile-series-expression)
    ((syntax:symbol) %compile-variable-reference)
    ((syntax:table) %compile-self-evaluating)
    ((syntax:table) %compile-self-evaluating)
    ((syntax:text) %compile-self-evaluating)
    ((syntax:true) %compile-self-evaluating)
    ((syntax:undefined) %compile-self-evaluating)
    (else #f)))

(define (%syntax-type expr)
  (car expr))

(define (%compile expr env)
  (let ((_compile (%get-compiler-for (%syntax-type expr)))) 
    (if _compile
        (_compile expr env)
        (error (str "Syntax error: " expr)))))
