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

(define (bard:initial-environment)
  (bard:extend-environment 
   (bard:null-environment)
   '+ (%make-method name: '+ signature: `((x ,<fixnum>) (y ,<fixnum>)) method-function: (lambda (x y)(+ x y)))))

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

(define (%test-eval str #!optional (env '()))
  (show (bard:eval (bard:read-from-string str) env)))

;;; (%test-eval "undefined" (bard:initial-environment))
;;; (%test-eval "nothing" (bard:initial-environment))
;;; (%test-eval "true" (bard:initial-environment))
;;; (%test-eval "false" (bard:initial-environment))
;;; (%test-eval "1" (bard:initial-environment))
;;; (%test-eval "888888888" (bard:initial-environment))
;;; (%test-eval "-888888888" (bard:initial-environment))
;;; (%test-eval "1.23" (bard:initial-environment))
;;; (%test-eval "-1.23" (bard:initial-environment))
;;; (%test-eval "-1/23" (bard:initial-environment))
;;; (%test-eval "\\c" (bard:initial-environment))
;;; (%test-eval "\\space" (bard:initial-environment))
;;; (%test-eval "Name:" (bard:initial-environment))
;;; (%test-eval "()" (bard:initial-environment))
;;; (%test-eval "\"foo Bar Baz\"" (bard:initial-environment))
;;; (%test-eval "+" (bard:initial-environment))
;;; (%test-eval "(+ 2 3)" (bard:initial-environment))
