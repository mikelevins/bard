;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          special.scm
;;;; Project:       Bard
;;;; Purpose:       bard special forms
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************


(define (%ensure-valid-quotation x)
  (if (and (list? x)
           (not (null? (cdr x)))
           (null? (cddr x)))
      x
      (error "invalid quote form")))

(define (%expand-quotation expr env)
  (cadr expr))

;;; (%expand-quotation '(quote *)(%initial-bard-environment))

(define $special-forms-table
  (->table 
   'and (lambda (expr env)
          (let loop ((expr (cdr expr))
                     (val '()))
            (if (null? expr)
                val
                (let ((v (%eval (car expr) env)))
                  (if v
                      (loop (cdr expr) v)
                      #f)))))
   'begin (lambda (expr env) 
            (let loop ((forms (cdr expr))
                       (val (%nothing)))
              (if (null? forms)
                  val
                  (let ((form (car forms)))
                    (loop (cdr forms)
                          (%eval form env))))))
   'define (lambda (expr env) (%define-variable (list-ref expr 1) (%eval (list-ref expr 2) env)))
   'define-function (lambda (expr env) 
                      (let* ((proto (cadr expr))
                             (body (cons 'begin (drop 2 expr)))
                             (fname (car proto))
                             (params (cdr proto))
                             (sig (%function-param-list->method-signature params env))
                             (formal-params (%function-param-list->formal-arguments params))
                             (fn-binding (%find-binding env fname))
                             (fn (if fn-binding
                                     (%binding-value fn-binding)
                                     (let ((f (%make-function name: fname)))
                                       (%set-binding! env fname f)
                                       f)))
                             (meth (%make-method name: fname params: formal-params body: body)))
                        (%function-add-method! fn sig meth)))
   'function (lambda (expr env) (%make-function name: #f))
   'if (lambda (expr env)
         (let ((test (list-ref expr 1))
               (conseq (list-ref expr 2))
               (alt? (> (length expr) 3)))
           (if (%eval test env)
               (%eval conseq env)
               (if alt?
                   (%eval (list-ref expr 3) env)
                   (%nothing)))))
   'let (lambda (expr env)
          (let ((body (drop 2 expr)))
            (let loop ((bindings (list-ref expr 1))
                       (env env))
              (if (null? bindings)
                  (let loop2 ((forms body)
                              (val (%nothing)))
                    (if (null? forms)
                        val
                        (let ((form (car forms)))
                          (loop2 (cdr forms)
                                 (%eval form env)))))
                  (let ((binding (car bindings)))
                    (loop (cdr bindings)
                          (%add-binding env (car binding)(%eval (cadr binding) env))))))))
   'method (lambda (expr env) (%make-method name: #f params: (list-ref expr 1) body: (cons 'begin (drop 2 expr))))
   'or (lambda (expr env)
         (let loop ((expr (cdr expr)))
           (if (null? expr)
               #f
               (let ((v (%eval (car expr) env)))
                 (if v
                     v
                     (loop (cdr expr)))))))
   'quote (lambda (expr env) (%expand-quotation (%ensure-valid-quotation expr) env))
   'set! (lambda (expr env) (%set-variable! (list-ref expr 1) (%eval (list-ref expr 2) env)  env))
   ))

(define (%special-form? expr)
  (and (table-ref $special-forms-table (car expr) #f)
       #t))

(define (%eval-special-form expr env)
  (let ((evaluator (table-ref $special-forms-table (car expr) #f)))
    (if evaluator
        (evaluator expr env)
        (error "unrecognized special form" (car expr)))))

