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
  (case (first expr)
    (else expr)))

(define (bard:compile expr #!optional (env '()))
  (cond
   ((symbol? expr) (bard:compile-variable expr env))
   ((null? expr)(bard:compile-constant expr))
   ((list? expr) (if (with-exit-form? expr)
                     (bard:compile-with-exit expr env)
                     (bard::compile-application expr env)))
   (else (bard:compile-constant expr))))


