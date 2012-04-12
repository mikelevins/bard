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

(define $special-forms-table
  (->table 
   'define (lambda (expr env) (%define-variable (list-ref expr 1) (%eval (list-ref expr 2) env)))
   'begin (lambda (expr env) 
            (let loop ((forms (cdr expr))
                       (val '()))
              (if (null? forms)
                  val
                  (let ((form (car forms)))
                    (loop (cdr forms)
                          (%eval form env))))))
   'if (lambda (expr env)
         (let ((test (list-ref expr 1))
               (conseq (list-ref expr 2))
               (alt? (> (length expr) 3)))
           (if (%eval test env)
               (%eval conseq env)
               (if alt?
                   (%eval (list-ref expr 3) env)
                   (bard:nothing)))))
   'method (lambda (expr env) (%make-method name: #f params: (list-ref expr 1) body: (drop 2 expr)))
   'function (lambda (expr env) (%make-function name: #f))
   'set! (lambda (expr env) (%set-variable! env (list-ref expr 1) (%eval (list-ref expr 2) env)))
   ))

(define (%special-form? expr)
  (and (table-ref $special-forms-table (car expr) #f)
       #t))

(define (%eval-special-form expr env)
  (let ((evaluator (table-ref $special-forms-table (car expr) #f)))
    (if evaluator
        (evaluator expr env)
        (error "unrecognized special form" (car expr)))))

