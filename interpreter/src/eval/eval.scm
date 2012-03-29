;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          eval.scm
;;;; Project:       Bard
;;;; Purpose:       the evaluator
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(define (bard:eval-variable var env)
  (let ((binding (bard:find-binding env var)))
    (if binding
        (cdr binding)
        (error "Undefined variable" var))))

(define (bard:eval-application-form expr env)
  (bard:apply (bard:eval (car expr) env)
              (map (lambda (x)(bard:eval x env))
                   (cdr expr))))

(define (bard:eval-list expr env)
  (cond
   ((bard:special-form? expr)(bard:eval-special-form expr env))
   ((bard:macro-form? expr)(bard:eval-macro-form expr env))
   (else (bard:eval-application-form expr env))))

(define (bard:eval expr #!optional (env (bard:top-level-environment)))
  (cond
   ((bard:undefined? expr) expr)
   ((bard:nothing? expr) expr)
   ((eq? expr (bard:true)) expr)
   ((eq? expr (bard:false)) expr)
   ((bard:number? expr) expr)
   ((bard:character? expr) expr)
   ((bard:keyword? expr) expr)
   ((bard:frame? expr) expr)
   ((bard:text? expr) expr)
   ((bard:symbol? expr) (bard:eval-variable expr env))
   ((bard:list? expr) (bard:eval-list expr env))
   (else (error "unrecognized expression" expr))))

