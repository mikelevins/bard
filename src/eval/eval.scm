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
        (%binding-value binding)
        (let ((global-val (%global-value var)))
          (if (%defined? global-val)
              global-val
              (error (string-append "Undefined variable: " (object->string var))))))))

(define (%eval-function-application expr env)
  (let ((expr (map (lambda (x)(%eval x env)) expr)))
    (%apply (car expr)(cdr expr))))

(define (%eval-application expr env)
  (cond
   ((%special-form? expr)(%eval-special-form expr env))
   ((%macro-form? expr)(%eval-macro-form expr env))
   (else (%eval-function-application expr env))))

(define (%eval expr env)
  (cond
   ((%symbol? expr) (%eval-variable expr env))
   ((%cons? expr) (%eval-application expr env))
   (else expr)))

(define (%test-eval str #!optional (env '()))
  (%eval (%read-from-string str) env))

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
;;; (%test-eval "#\\c" (%initial-bard-environment))
;;; (%test-eval "#\\space" (%initial-bard-environment))
;;; (%test-eval "Name:" (%initial-bard-environment))
;;; (%test-eval "()" (%initial-bard-environment))
;;; (%test-eval "\"foo Bar Baz\"" (%initial-bard-environment))
;;; (%test-eval "+" (%initial-bard-environment))
;;; (%test-eval "(+ 2 3)" (%initial-bard-environment))
;;; (%test-eval "(* (- 4 2)(+ 1 2))" (%initial-bard-environment))
;;; (%test-eval "(method (x)(+ x 1))" (%initial-bard-environment))
