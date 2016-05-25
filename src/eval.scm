;;;; ***********************************************************************
;;;;
;;;; Name:          eval.scm
;;;; Project:       Bard
;;;; Purpose:       bard's evaluator
;;;; Author:        mikel evins
;;;; Copyright:     2016 by mikel evins
;;;;
;;;; ***********************************************************************

(declare (extended-bindings))

(define (%defined? val)
  (not (eqv? val #!unbound)))

(define (%eval-function-application expr env)
  (let* ((op (%eval (car expr) env))
         (args (map (lambda (x)(%eval x env))
                    (cdr expr))))
    (%apply op args)))

(define (%eval-application expr env)
  (cond
   ((%special-form? expr)(%eval-special-form expr env))
   ((%macro-form? expr)(%eval-macro-form expr env))
   (else (%eval-function-application expr env))))

(define (%eval-variable var env)
  (if (eq? var 'undefined)
      #!unbound
      (let ((val (%lookup-variable-value env var)))
        (if (%defined? val)
            val
            (let ((global-val (%global-value var)))
              (if (%defined? global-val)
                  global-val
                  #!unbound))))))

(define (bard:eval expr #!optional (env '()))
  (cond
   ((symbol? expr) (%eval-variable expr env))
   ((pair? expr) (%eval-application expr env))
   (else expr)))

