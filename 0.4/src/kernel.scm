;;;; ***********************************************************************
;;;;
;;;; Name:          kernel.scm
;;;; Project:       Bard
;;;; Purpose:       the kernel evaluator
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(declare (extended-bindings))

(define (with-exit-form? expr)
  (and (list? expr)
       (not (null? expr))
       (eq? 'with-exit (first expr))))

(define (kernel:eval-variable expr env)
  (if (globals:bound? expr)
      (globals:ref expr)
      (env:ref env expr)))

(define (kernel:eval-with-exit expr env)
  (let* ((form (cdr expr))
         (exit-var (car (car form)))
         (body (cons 'begin (drop 1 form))))
    (call/cc (lambda (k)(kernel:eval body (env:add-binding env exit-var k))))))

(define (kernel:eval-application expr env)
  (not-yet-implemented 'kernel:eval-application))

(define (kernel:eval-constant expr)
  expr)

(define (kernel:eval expr #!optional (env '()))
  (cond
   ((symbol? expr) (kernel:eval-variable expr env))
   ((list? expr) (if (with-exit-form? expr)
                     (kernel:eval-with-exit expr env)
                     (kernel:eval-application expr env)))
   (else (kernel:eval-constant expr))))
