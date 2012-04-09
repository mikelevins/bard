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

(define (%initial-bard-environment)
  (%extend-environment 
   (%null-environment)
   '+ (lambda args (apply + args))))

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
