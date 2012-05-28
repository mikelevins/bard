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

(define $special-forms-table (make-table test: eq?))

(define (%defspecial nm eval-fn)
  (table-set! $special-forms-table nm eval-fn))

(define (%special-evaluator nm)
  (table-ref $special-forms-table nm #f))

(define (%special-form? expr)
  (and (table-ref $special-forms-table (%car expr) #f)
       #t))

(define (%eval-special-form expr env)
  (let ((evaluator (table-ref $special-forms-table (%car expr) #f)))
    (if evaluator
        (evaluator expr env)
        (error (string-append "unrecognized special form" (%as-string (%car expr)))))))

(%defspecial 'and 
             (lambda (expr env)
               (let loop ((expr (%cdr expr))
                          (val (%true)))
                 (if (%null? expr)
                     val
                     (let ((v (%eval (%car expr) env)))
                       (if (%true? v)
                           (loop (%cdr expr) v)
                           (%false)))))))

(%defspecial 'begin
             (lambda (expr env) 
               (let loop ((forms (%cdr expr))
                          (val (%nothing)))
                 (if (%null? forms)
                     val
                     (let ((form (%car forms)))
                       (loop (%cdr forms)
                             (%eval form env)))))))

(%defspecial 'define
             (lambda (expr env)
               (%defglobal (%list-ref expr 1) (%eval (%list-ref expr 2) env))
               (%list-ref expr 1)))

(%defspecial 'if
             (lambda (expr env)
               (let ((test (%list-ref expr 1))
                     (conseq (%list-ref expr 2))
                     (alt? (> (%length expr) 3)))
                 (if (%eval test env)
                     (%eval conseq env)
                     (if alt?
                         (%eval (%list-ref expr 3) env)
                         (%nothing))))))

(%defspecial 'let
             (lambda (expr env)
               (let ((body (%drop 2 expr)))
                 (let loop ((bindings (%list-ref expr 1))
                            (env env))
                   (if (%null? bindings)
                       (let loop2 ((forms body)
                                   (val (%nothing)))
                         (if (%null? forms)
                             val
                             (let ((form (%car forms)))
                               (loop2 (%cdr forms)
                                      (%eval form env)))))
                       (let ((binding (%car bindings)))
                         (loop (%cdr bindings)
                               (%add-binding env
                                             (%car binding)
                                             (%eval (%car (%cdr binding))
                                                    env)))))))))

(%defspecial 'not
             (lambda (expr env)
               (if (%eval (%car (%cdr expr)) env)
                   (%false)
                   (%true))))

(%defspecial 'or
             (lambda (expr env)
               (let loop ((expr (%cdr expr)))
                 (if (%null? expr)
                     (%false)
                     (let ((v (%eval (%car expr) env)))
                       (if v
                           v
                           (loop (%cdr expr))))))))

(%defspecial 'quote (lambda (expr env)
                      (if (= 2 (%length expr))
                          (%car (%cdr expr))
                          (error (string-append "Wrong number of arguments to quote: " (%as-string (%cdr expr)))))))

(%defspecial 'set!
             (lambda (expr env)
               (%set-variable! (%list-ref expr 1)
                               (%eval (%list-ref expr 2)
                                      env)
                               env)))

(%defspecial 'time
             (lambda (expr env)
               (time (%eval (%car (%cdr expr)) env))))
