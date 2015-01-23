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

(define (kernel:eval-function-application expr env)
  (not-yet-implemented 'kernel:eval-function-application))

(define (kernel:eval-lambda expr env)
  (not-yet-implemented 'kernel:eval-lambda))

(define (kernel:eval-begin expr env)
  (not-yet-implemented 'kernel:eval-begin))

(define (kernel:eval-cond expr env)
  (not-yet-implemented 'kernel:eval-cond))

(define (kernel:eval-define expr env)
  (not-yet-implemented 'kernel:eval-define))

(define (kernel:eval-ensure expr env)
  (not-yet-implemented 'kernel:eval-ensure))

(define (kernel:eval-if expr env)
  (not-yet-implemented 'kernel:eval-if))

(define (kernel:eval-quote expr env)
  (not-yet-implemented 'kernel:eval-quote))

(define (kernel:eval-set! expr env)
  (not-yet-implemented 'kernel:eval-set!))

(define (kernel:eval-application expr env)
  (case (first expr)
    ((^)(kernel:eval-lambda expr env))
    ((begin)(kernel:eval-begin expr env))
    ((cond)(kernel:eval-cond expr env))
    ((define)(kernel:eval-define expr env))
    ((ensure)(kernel:eval-ensure expr env))
    ((if)(kernel:eval-if expr env))
    ((quote)(kernel:eval-quote expr env))
    ((set!)(kernel:eval-set! expr env))
    ((with-exit)(kernel:eval-with-exit expr env))
    (else (kernel:eval-function-application expr env))))

(define (kernel:eval-constant expr)
  expr)

(define (kernel:eval expr #!optional (env '()))
  (cond
   ((symbol? expr) (kernel:eval-variable expr env))
   ((list? expr) (if (with-exit-form? expr)
                     (kernel:eval-with-exit expr env)
                     (kernel:eval-application expr env)))
   (else (kernel:eval-constant expr))))

