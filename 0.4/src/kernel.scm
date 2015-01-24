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
  (let ((binding (env:get-binding expr env)))
    (if binding
        (binding-value binding)
        (if (globals:bound? expr)
            (globals:ref expr)
            (error "Unbound variable" expr)))))

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
  (let loop ((exprs (cdr expr))
             (result (nothing)))
    (if (null? exprs)
        result
        (loop (cdr exprs)
              (kernel:eval (car exprs))))))

(define (kernel:eval-cond expr env)
  (not-yet-implemented 'kernel:eval-cond))

(define (kernel:eval-define expr env)
  (let ((var (cadr expr))
        (val-expr (caddr expr)))
    (globals:set! var (kernel:eval val-expr env))))

(define (kernel:eval-ensure expr env)
  (not-yet-implemented 'kernel:eval-ensure))

(define (kernel:eval-if expr env)
  (let ((test-val (kernel:eval (second expr) env)))
    (if (true? test-val)
        (kernel:eval (third expr) env)
        (kernel:eval (fourth expr) env))))

(define (kernel:eval-quote expr env)
  (cadr expr))

(define (kernel:eval-set! expr env)
  (let* ((var (cadr expr))
         (val-expr (caddr expr))
         (binding (env:get-binding var env)))
    (if binding
        (binding-set! binding (kernel:eval val-expr env))
        (if (globals:bound? var)
            (globals:set! var (kernel:eval val-expr env))
            (error "Undefined variable" expr)))))

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
   ((null? expr)(kernel:eval-constant expr))
   ((list? expr) (if (with-exit-form? expr)
                     (kernel:eval-with-exit expr env)
                     (kernel:eval-application expr env)))
   (else (kernel:eval-constant expr))))

