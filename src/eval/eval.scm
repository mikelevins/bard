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
  (if (eq? var 'undefined)
      (%undefined)
      (let ((val (%lookup-variable-value env var)))
        (if (%defined? val)
            val
            (let ((global-val (%global-value var)))
              (if (%defined? global-val)
                  global-val
                  (error (string-append "Undefined variable: " (object->string var)))))))))

(define (%eval-function-application expr env)
  (let ((expr (%map (lambda (x)(%eval x env)) expr)))
    (%apply (%car expr)(%cdr expr))))

(define (%eval-application expr env)
  (cond
   ((%special-form? expr)(%eval-special-form expr env))
   ((%macro-form? expr)(%eval-macro-form expr env))
   (else (%eval-function-application expr env))))

(define (%eval expr #!optional (env '()))
  (cond
   ((%symbol? expr) (%eval-variable expr env))
   ((%list? expr) (%eval-application expr env))
   (else expr)))

