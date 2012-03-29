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
   'define (lambda (expr env) (bard:define-variable (list-ref expr 1) (bard:eval (list-ref expr 2) env)))
   'begin (lambda (expr env) 
            (let loop ((forms (cdr expr))
                       (val '()))
              (if (null? forms)
                  val
                  (let ((form (car forms)))
                    (loop (cdr forms)
                          (bard:eval form env))))))
   'if (lambda (expr env)
         (let ((test (list-ref expr 1))
               (conseq (list-ref expr 2))
               (alt? (> (length expr) 3)))
           (if (bard:eval test env)
               (bard:eval conseq env)
               (if alt?
                   (bard:eval (list-ref expr 3) env)
                   bard:nothing))))
   'method (lambda (expr env) (%make-interpreted-method name: #f signature: (list-ref expr 1) method-function: (drop 2 expr)))
   'function (lambda (expr env) (%make-function name: #f signature: '()))
   'set! (lambda (expr env) (bard:set-variable! env (list-ref expr 1) (bard:eval (list-ref expr 2) env)))
   ))

(define (bard:special-form? expr)
  (and (table-ref $special-forms-table (car expr) #f)
       #t))

(define (bard:eval-special-form expr env)
  (let ((evaluator (table-ref $special-forms-table (car expr) #f)))
    (evaluator expr env)))


