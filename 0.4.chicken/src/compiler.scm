;;;; ***********************************************************************
;;;;
;;;; Name:          compiler.scm
;;;; Project:       Bard
;;;; Purpose:       the bard compiler
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

;;; ---------------------------------------------------------------------
;;; ABOUT
;;; ---------------------------------------------------------------------
;;; converts expressions in bard to kernel expressions
;;; by folding constant expressions, expanding all macros,
;;; and rewriting expressions that use the more complex
;;; and powerful constructs of full bard as expressions
;;; that use only the kernel language.

;;; ---------------------------------------------------------------------
;;; macros
;;; ---------------------------------------------------------------------

(define *bard-macro-forms* (make-hash-table))

;;; an expander has the form (lambda (expr) ...) => expr*
(define (define-bard-macro mname expander)
  (hash-table-set! *bard-macro-forms* mname expander))

(define (bard:get-macro-expander mname)
  (hash-table-ref/default *bard-macro-forms* mname #f))

(define (bard:macro-form? expr)
  (bard:get-macro-expander (car expr)))

(define (bard:macroexpand expr)
  (let ((expand (bard:get-macro-expander (car expr))))
    (if expand
        (lambda:apply expand `(,expr))
        (error "Undefined macro" (car expr)))))

;;; (macro mname (^ (expr) ...))
(define (bard:compile-macro-definition expr)
  (let ((mname (list-ref expr 1))
        (mexpander (kernel:eval (bard:compile (list-ref expr 2)))))
    (define-bard-macro mname mexpander)))

;;; ---------------------------------------------------------------------
;;; compilers for special forms
;;; ---------------------------------------------------------------------

(define (bard:compile-loop expr env) (not-yet-implemented 'bard:compile-loop))

;;; ---------------------------------------------------------------------
;;; main compiler
;;; ---------------------------------------------------------------------

(define (bard:compile-variable expr env)
  expr)

(define (bard:compile-constant expr)
  expr)

(define (bard:compile-with-exit expr env)
  (let ((exit-form (list-ref expr 1)))
    `(with-exit ,exit-form
                ,@(map (lambda (x)(bard:compile x env))
                       (drop 2 expr)))))

(define (bard::compile-application expr env)
  (if (bard:macro-form? expr)
      (bard:compile (bard:macroexpand expr) env)
      (let ((comp (lambda (ex) (bard:compile ex env))))
        (case (first expr)
          ((^) `(FN ,(cadr expr) ,@(map comp (cddr expr))))
          ((begin) `(BEGIN ,@(map comp (cdr expr))))
          ((cond) `(COND ,@(map comp (cdr expr))))
          ((define)(bard:compile-define expr env))
          ((ensure) `(ENSURE ,@(map comp (cdr expr))))
          ((if) `(IF ,@(map comp (cdr expr))))
          ((let)(bard:compile-let expr env))
          ((loop) (bard:compile-loop expr env))
          ((macro)(bard:compile-macro-definition expr))
          ((quasiquote) (bard:compile (%expand-quasiquote (cadr expr) 0) env))
          ((set!)(bard:compile-set! expr env))
          ((unquote) (error "Invalid context for unquote"))
          ((unquote-splicing) (error "Invalid context for unquote-splicing"))
          ((with-exit)(bard:compile-with-exit expr env))
          (else (map (lambda (e)(bard:compile e env)) expr))))))

(define (bard:compile expr #!optional (env '()))
  (cond
   ((symbol? expr) (bard:compile-variable expr env))
   ((null? expr)(bard:compile-constant expr))
   ((list? expr) (if (with-exit-form? expr)
                     (bard:compile-with-exit expr env)
                     (bard::compile-application expr env)))
   (else (bard:compile-constant expr))))


