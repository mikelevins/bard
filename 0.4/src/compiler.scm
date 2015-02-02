;;;; ***********************************************************************
;;;;
;;;; Name:          compiler.scm
;;;; Project:       Bard
;;;; Purpose:       the bard compiler
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(declare (extended-bindings))

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

(define (bard:macro-form? expr) #f)
(define (bard:macroexpand expr)(not-yet-implemented 'bard:macroexpand))

;;; ---------------------------------------------------------------------
;;; compilers for special forms
;;; ---------------------------------------------------------------------

(define (bard:compile-define expr env) (not-yet-implemented 'bard:compile-define))
(define (bard:compile-loop expr env) (not-yet-implemented 'bard:compile-loop))
(define (bard:compile-macro expr env) (not-yet-implemented 'bard:compile-macro))
(define (bard:compile-with-exit expr env)(not-yet-implemented 'bard:compile-with-exit))

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
      (bard::compile-application (bard:macroexpand expr) env)
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
          ((macro)(bard:compile-macro expr env))
          ((quote) `(QUOTE ,@(cdr expr)))
          ((set!)(bard:compile-set! expr env))
          ((with-exit)(bard:compile-with-exit expr env))
          (else expr)))))

(define (bard:compile expr #!optional (env '()))
  (cond
   ((symbol? expr) (bard:compile-variable expr env))
   ((null? expr)(bard:compile-constant expr))
   ((list? expr) (if (with-exit-form? expr)
                     (bard:compile-with-exit expr env)
                     (bard::compile-application expr env)))
   (else (bard:compile-constant expr))))


