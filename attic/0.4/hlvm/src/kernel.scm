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
       (eq? 'WITH-EXIT (first expr))))

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
         (body (bard:compile (cons 'begin (drop 1 form)) env)))
    (call/cc (lambda (k)
               (kernel:eval body
                            (env:add-binding env exit-var
                                             (make-native-method k)))))))

(define (kernel:eval-function-application expr env)
  (let ((op (kernel:eval (first expr) env))
        (args (map (lambda (e)(kernel:eval e env))
                   (rest expr))))
    (cond
     ((function? op)(apply-function op args))
     ((method? op)(apply-method op args))
     ((native-method? op)(apply (native-method-procedure op) args))
     ((continuation? op)(apply op args))
     (else (error "Unrecognized procedure type " op)))))

;;; (FUNCTION input-types output-types)
(define (kernel:eval-function expr env)
  (let ((input-types (map (lambda (i)(kernel:eval i env))
                          (list-ref expr 1)))
        (output-types (map (lambda (j)(kernel:eval j env))
                           (list-ref expr 2))))
    (make-function input-types output-types '())))

(define (kernel:eval-method expr env)
  (let ((lambda-list (list-ref expr 1))
        (body (cons 'BEGIN (drop 2 expr))))
    (make-method lambda-list body env)))

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

(define (kernel:eval-def expr env)
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

(define (kernel:eval-assign! expr env)
  (let* ((var (cadr expr))
         (val-expr (caddr expr))
         (binding (env:get-binding var env)))
    (if binding
        (env:binding-set! binding (kernel:eval val-expr env))
        (if (globals:bound? var)
            (globals:set! var (kernel:eval val-expr env))
            (error "Undefined variable" expr)))))

(define (kernel:eval-repeat expr env)
  (let ((op (cadr expr)))
    (let loop ()
      (kernel:eval op env)
      (loop))))

(define (kernel:eval-time expr env)
  (let ((op (cadr expr)))
    (time (kernel:eval op env))))

;;; (CC (METHOD (k) ...))
(define (kernel:eval-cc expr env)
  (let* ((fn (cadr expr))
         (kvar (car (cadr fn)))
         (body (drop 2 fn)))
    (call/cc
     (lambda (k)
       (kernel:eval `(BEGIN ,@body)
                    (env:add-binding env kvar k))))))

(define (kernel:eval-application expr env)
  (case (first expr)
    ((ASSIGN)(kernel:eval-assign! expr env))
    ((BEGIN)(kernel:eval-begin expr env))
    ((CC)(kernel:eval-cc expr env))
    ((COND)(kernel:eval-cond expr env))
    ((DEF)(kernel:eval-def expr env))
    ((ENSURE)(kernel:eval-ensure expr env))
    ((FUNCTION)(kernel:eval-function expr env))
    ((METHOD)(kernel:eval-method expr env))
    ((IF)(kernel:eval-if expr env))
    ((quote)(kernel:eval-quote expr env))
    ((repeat)(kernel:eval-repeat expr env))
    ((time)(kernel:eval-time expr env))
    ((WITH-EXIT)(kernel:eval-with-exit expr env))
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
