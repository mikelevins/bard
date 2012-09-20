;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          front.scm
;;;; Project:       Bard
;;;; Purpose:       the Bard compiler front-end
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(define (bard:compile-null expr) '(nothing))

(define (bard:compile-variable-reference expr env) 
  (if (lookup expr env)
      `(lref ',expr ',env)
      `(gref ',expr)))

(define (self-evaluating? expr)
  (or (null? expr)
      (boolean? expr)
      (char? expr)
      (number? expr)
      (vector? expr)
      (eq? expr #!eof)
      (eq? expr #!unbound)))

(define (bard:compile-self-evaluating expr)
  `(constant ',expr))

(define (special-form? op)
  (member op '(^ begin cond define if let loop
                 match method setter unless when)))

(define (bard:compile-method expr env) #f)
(define (bard:compile-begin expr env) #f)
(define (bard:compile-cond expr env) #f)
(define (bard:compile-define expr env) #f)
(define (bard:compile-if expr env) #f)
(define (bard:compile-let expr env) #f)
(define (bard:compile-loop expr env) #f)
(define (bard:compile-match expr env) #f)
(define (bard:compile-setter expr env) #f)
(define (bard:compile-unless expr env) #f)

(define (bard:compile-special-form expr env)
  (case (car expr)
    ((^ method)(bard:compile-method expr env))
    ((begin)(bard:compile-begin expr env))
    ((cond)(bard:compile-cond expr env))
    ((define)(bard:compile-define expr env))
    ((if)(bard:compile-if expr env))
    ((let)(bard:compile-let expr env))
    ((loop)(bard:compile-loop expr env))
    ((match)(bard:compile-match expr env))
    ((setter)(bard:compile-setter expr env))
    ((unless)(bard:compile-unless expr env))
    (else (error (str "Syntax error; unrecognized special form: "
                      expr)))))

(define (macro? op) #f)

(define (bard:macroexpand expr) #f)

(define (bard:compile-funcall expr env) #f)

(define (bard:compile-list expr env)
  (let ((op (car expr)))
    (cond
     ((special-form? op)(bard:compile-special-form expr env))
     ((macro? op)(bard:compile (bard:macroexpand expr) env))
     (else (bard:compile-funcall expr env)))))

(define (bard:compiler-syntax-error expr)
  (error (str "Syntax error; unrecognized expression type: "
              expr)))

(define (bard:compile expr env)
  (cond
   ((null? expr)(bard:compile-null expr))
   ((symbol? expr)(bard:compile-variable-reference expr env))
   ((self-evaluating? expr)(bard:compile-self-evaluating expr))
   ((pair? expr)(bard:compile-list expr env))
   (else (bard:compiler-syntax-error expr))))
