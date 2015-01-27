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
        (env:binding-value binding)
        (if (globals:bound? expr)
            (globals:ref expr)
            (error "Unbound variable" expr)))))

(define (kernel:eval-with-exit expr env)
  (let* ((form (cdr expr))
         (exit-var (car (car form)))
         (body (cons 'begin (drop 1 form))))
    (call/cc (lambda (k)(kernel:eval body (env:add-binding env exit-var k))))))

(define (kernel:eval-function-application expr env)
  (let ((op (kernel:eval (first expr) env))
        (args (map (lambda (e)(kernel:eval e env))
                   (rest expr))))
    (cond ((lambda:native-method? op)(apply (lambda:method-native-function op) args))
          ((kernel-lambda? op) (lambda:apply op args))
          (else (error "Unrecognized function type" expr)))))

(define (kernel:eval-lambda expr env)
  (let ((lambda-list (list-ref expr 1))
        (body (cons 'begin (drop 2 expr))))
    (lambda:create lambda-list body env)))

(define (eval-sequence exprs env)
  (let loop ((items exprs)
             (result (nothing)))
    (if (null? items)
        result
        (loop (cdr items)
              (kernel:eval (car items) env)))))

(define (kernel:eval-begin expr env)
  (let ((exprs (cdr expr)))
    (eval-sequence exprs env)))

(define (kernel:eval-cond expr env)
  (let loop ((clauses (cdr expr)))
    (if (null? clauses)
        (nothing)
        (let* ((clause (car clauses))
               (testval (kernel:eval (car clause) env)))
          (if (true? testval)
              (eval-sequence (cdr clause) env)
              (loop (cdr clauses)))))))

(define (kernel:eval-define expr env)
  (let ((var (cadr expr))
        (val-expr (caddr expr)))
    (globals:set! var (kernel:eval val-expr env))))

(define (parse-ensure-form expr)
  (let ((after-pos (position-if (lambda (x)(eqv? x after:))
                                expr)))
    (if after-pos
        (if (eqv? 2 after-pos)
            (let ((cleanup-form (list-ref expr 1))
                  (body-forms (drop 3 expr)))
              (values cleanup-form body-forms))
            (error "Malformed ensure form"))
        (error "Missing after: in ensure form"))))

;;; (ensure cleanup-form
;;;         after: expr1 expr2 ...)
(define (kernel:eval-ensure expr env)
  (receive (cleanup body)(parse-ensure-form expr)
           (dynamic-wind (lambda () #f)
               (lambda () (eval-sequence body env))
               (lambda () (kernel:eval cleanup env)))))

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

(define (kernel:read line)
  (reader:string->object line))

(define (kernel:print object)
  (display (printer:object->string object)))

