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

(define (%eval-variable var env)
  (let ((binding (%find-binding env var)))
    (if binding
        (cdr binding)
        (error "Undefined variable" var))))

(define (%eval-application-form expr env)
  (%apply (%eval (car expr) env)
          (map (lambda (x)(%eval x env))
               (cdr expr))))

(define (%eval-list expr env)
  (cond
   ((%special-form? expr)(%eval-special-form expr env))
   ((%macro-form? expr)(%eval-macro-form expr env))
   (else (%eval-application-form expr env))))

(define (%eval expr #!optional (env (%top-level-environment)))
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
   ((procedure? expr) expr)
   ((%method? expr) expr)
   ((%function? expr) expr)
   ((bard:symbol? expr) (%eval-variable expr env))
   ((bard:cons? expr) (%eval-list expr env))
   (else (error "unrecognized expression" expr))))

(define (%test-eval str #!optional (env '()))
  (show (%eval (bard:read-from-string str) env)))

;;; (%test-eval "undefined" (%initial-bard-environment))
;;; (%test-eval "nothing" (%initial-bard-environment))
;;; (%test-eval "true" (%initial-bard-environment))
;;; (%test-eval "false" (%initial-bard-environment))
;;; (%test-eval "1" (%initial-bard-environment))
;;; (%test-eval "888888888" (%initial-bard-environment))
;;; (%test-eval "-888888888" (%initial-bard-environment))
;;; (%test-eval "1.23" (%initial-bard-environment))
;;; (%test-eval "-1.23" (%initial-bard-environment))
;;; (%test-eval "-1/23" (%initial-bard-environment))
;;; (%test-eval "\\c" (%initial-bard-environment))
;;; (%test-eval "\\space" (%initial-bard-environment))
;;; (%test-eval "Name:" (%initial-bard-environment))
;;; (%test-eval "()" (%initial-bard-environment))
;;; (%test-eval "\"foo Bar Baz\"" (%initial-bard-environment))
;;; (%test-eval "+" (%initial-bard-environment))
;;; (%test-eval "(+ 2 3)" (%initial-bard-environment))
;;; (%test-eval "(* (- 4 2)(+ 1 2))" (%initial-bard-environment))
