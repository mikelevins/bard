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
(define (bard:compile-quasiquote expr env) (not-yet-implemented 'bard:compile-quasiquote))
(define (bard:compile-receive expr env) (not-yet-implemented 'bard:compile-receive))
(define (bard:compile-repeat expr env) (not-yet-implemented 'bard:compile-repeat))
(define (bard:compile-send expr env) (not-yet-implemented 'bard:compile-send))

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
      (case (first expr)
        ((define)(bard:compile-define expr env))
        ((let)(bard:compile-let expr env))
        ((loop)(bard:compile-loop expr env))
        ((macro)(bard:compile-macro expr env))
        ((quasiquote)(bard:compile-quasiquote expr env))
        ((receive)(bard:compile-receive expr env))
        ((repeat)(bard:compile-repeat expr env))
        ((send)(bard:compile-send expr env))
        ((set!)(bard:compile-set! expr env))
        (else expr))))

(define (bard:compile expr #!optional (env '()))
  (cond
   ((symbol? expr) (bard:compile-variable expr env))
   ((null? expr)(bard:compile-constant expr))
   ((list? expr) (if (with-exit-form? expr)
                     (bard:compile-with-exit expr env)
                     (bard::compile-application expr env)))
   (else (bard:compile-constant expr))))


