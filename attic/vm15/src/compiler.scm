;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          compiler.scm
;;;; Project:       Bard
;;;; Purpose:       bard compiler 
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

;;; ---------------------------------------------------------------------
;;; code generation
;;; ---------------------------------------------------------------------

(define (bard:gen . args) `(,args))
(define (bard:gen-label) `(LABEL ,(gensym)))

;;; ---------------------------------------------------------------------
;;; special forms
;;; ---------------------------------------------------------------------

(define (bard:compile-begin args env)
  (let loop ((args args)
             (result '()))
    (if (null? args)
        (reverse result)
        (if (null? (cdr args))
            (loop (cdr args)
                  (append (bard:compile (car args) env)
                        result))
            (loop (cdr args)
                  (append (bard:gen 'POP)
                          (append (bard:compile (car args) env)
                                  result)))))))

(define (bard:compile-cond expr env) 
  (let* ((continue-return (bard:gen-label))
         (clauses (map (lambda (c)
                         (let ((test (bard:compile (car c) env))
                               (then-body (bard:compile (cadr c) env))
                               (continue-then (bard:gen-label))
                               (continue-end-clause (bard:gen-label)))
                           (append test 
                                   (bard:gen 'TJUMP continue-then)
                                   (bard:gen 'FJUMP continue-end-clause)
                                   (list continue-then)
                                   then-body
                                   (bard:gen 'JUMP continue-return)
                                   (list continue-end-clause))))
                       expr)))
    (append
     (apply append clauses)
     (list continue-return))))

(define (bard:params->signature params)
  (list 'signature
        (map (lambda (p)
               (cond
                ((symbol? p) 'Anything)
                ((list? p) (cadr p))
                (else (error "Malformed method parameter" p))))
             params)))

(define (bard:params->formals params)
  (list 'formals
        (map (lambda (p)
               (cond
                ((symbol? p) p)
                ((list? p) (car p))
                (else (error "Malformed method parameter" p))))
             params)))

(define (bard:make-method formals code env)
  (list 'method formals code env))

;;; (define-function (fname arg1 arg2...) expr1 expr2 ...)
(define (bard:compile-define-function expr env) 
  (let* ((proto (car expr))
         (fname (car proto))
         (params (cdr proto))
         (signature (bard:params->signature params))
         (formals (bard:params->formals params))
         (body (cdr expr))
         (code (bard:compile-begin body env))
         (method (bard:make-method formals code env)))
    (append
     (bard:gen 'CONST method)
     (bard:gen 'CONST fname)
     (bard:gen 'ADDM))))

(define (bard:compile-define-macro expr env) 
  #f)

(define (bard:make-protocol pname)(list 'protocol pname))

(define (bard:protocol-add-function! protocol fn)
  (set-cdr! protocol
            (append (cdr protocol)
                    (list fn))))

(define (make-function fn-description #!optional (protocol #f))
  (list 'function fn-description protocol))

(define (bard:compile-define-protocol expr env) 
  (let* ((pname (car expr))
         (protocol (bard:make-protocol pname))
         (pclause-forms (cdr expr))
         (pclauses (map (lambda (c)(make-function c pname)) pclause-forms)))
    (for-each (lambda (pc)(bard:protocol-add-function! protocol pc))
              pclauses)
    (append (bard:gen 'CONST protocol)
            (bard:gen 'DEF pname))))

(define (bard:compile-define-schema expr env) 
  #f)

(define (bard:compile-define-variable expr env) 
  (let ((var-form (car expr))
        (val-form (cadr expr)))
    (append (bard:compile val-form env)
            (bard:gen 'DEF var-form))))

(define (bard:compile-define-vector expr env) 
  #f)

(define (bard:compile-define expr env) 
  (let ((op (car expr)))
    (case op
      ((function)(bard:compile-define-function (cdr expr) env))
      ((macro)(bard:compile-define-macro (cdr expr) env))
      ((protocol)(bard:compile-define-protocol (cdr expr) env))
      ((schema)(bard:compile-define-schema (cdr expr) env))
      ((variable)(bard:compile-define-variable (cdr expr) env))
      ((vector)(bard:compile-define-vector (cdr expr) env))
      (else (error (str "Unrecognized definition type: define " op))))))

(define (bard:compile-if expr env) 
  (let ((test (bard:compile (car expr) env))
        (then-body (bard:compile (cadr expr) env))
        (else-body (bard:compile (caddr expr) env))
        (continue-then (bard:gen-label))
        (continue-else (bard:gen-label))
        (continue-end (bard:gen-label)))
    (append
     test
     (bard:gen 'TJUMP continue-then)
     (bard:gen 'FJUMP continue-else)
     (list continue-then)
     then-body
     (bard:gen 'JUMP continue-end)
     (list continue-else)
     else-body
     (bard:gen 'JUMP continue-end)
     (list continue-end))))

(define (bard:compile-let expr env) 
  #f)

(define (bard:compile-loop expr env) 
  #f)

(define (bard:compile-match expr env) 
  #f)

(define (bard:compile-method expr env) 
  #f)

(define (bard:compile-unless expr env) 
  (let ((test (bard:compile (car expr) env))
        (body (bard:compile-begin (cdr expr) env))
        (continue-before (bard:gen-label))
        (continue-after (bard:gen-label)))
    (append
     test
     (bard:gen 'FJUMP continue-before)
     (bard:gen 'JUMP continue-after)
     (list continue-before)
     body
     (list continue-after))))

(define (bard:compile-when expr env) 
  (let ((test (bard:compile (car expr) env))
        (body (bard:compile-begin (cdr expr) env))
        (continue-before (bard:gen-label))
        (continue-after (bard:gen-label)))
    (append
     test
     (bard:gen 'TJUMP continue-before)
     (bard:gen 'JUMP continue-after)
     (list continue-before)
     body
     (list continue-after))))

(define $bard-special-forms 
  (list->table
   `((begin . ,(lambda (expr env)(bard:compile-begin (cdr expr) env)))
     (cond . ,(lambda (expr env)(bard:compile-cond (cdr expr) env)))
     (define . ,(lambda (expr env)(bard:compile-define (cdr expr) env)))
     (if . ,(lambda (expr env)(bard:compile-if (cdr expr) env)))
     (let . ,(lambda (expr env)(bard:compile-let (cdr expr) env)))
     (loop . ,(lambda (expr env)(bard:compile-loop (cdr expr) env)))
     (match . ,(lambda (expr env)(bard:compile-match (cdr expr) env)))
     (method . ,(lambda (expr env)(bard:compile-method (cdr expr) env)))
     (unless . ,(lambda (expr env)(bard:compile-unless (cdr expr) env)))
     (when . ,(lambda (expr env)(bard:compile-when (cdr expr) env))))))

(define (special-form? expr)
  (table-ref $bard-special-forms (car expr) #f))

(define (bard:compile-special-form expr env)
  (let ((compiler (table-ref $bard-special-forms (car expr) #f)))
    (compiler expr env)))

;;; ---------------------------------------------------------------------
;;; macros
;;; ---------------------------------------------------------------------

(define $bard-macro-forms (make-table test: eq?))

(define (macro-form? expr)
  (table-ref $bard-macro-forms (car expr) #f))

(define (bard:macroexpand expr)
  (let ((expander (table-ref $bard-special-forms (car expr) #f)))
    (expander expr)))

;;; ---------------------------------------------------------------------
;;; primitives
;;; ---------------------------------------------------------------------

(define $primitives (make-table test: eq?))

(define (defprim pname p)
  (table-set! $primitives pname p))

(define (primitive? x)
  (table-ref $primitives x #f))

(define (bard:compile-primitive-call expr env)
  (let* ((op (car expr))
         (argforms (cdr expr))
         (args (map (lambda (arg)(bard:compile args env))
                    argforms)))
    (append args (bard:gen 'PRIM op))))

;;; ---------------------------------------------------------------------
;;; funcalls
;;; ---------------------------------------------------------------------

(define (bard:compile-function-call expr env)
  (let* ((op (car expr))
         (argforms (cdr expr))
         (args (map (lambda (arg)(bard:compile args env))
                    argforms)))
    (let ((continue (bard:gen-label)))
      (append
       (bard:gen 'SAVE continue)
       args
       (bard:compile op env)
       (bard:gen 'CALLJ (length args))
       continue))))

(define (bard:compile-funcall expr env)
  (if (primitive? (car expr))
      (bard:compile-primitive-call expr env)
      (bard:compile-function-call expr env)))

;;; ---------------------------------------------------------------------
;;; setters
;;; ---------------------------------------------------------------------
;;; TODO: implement setters of the form
;;; (setter (foo bar))

(define (setter-form? expr)
  (and (pair? expr)
       (= 2 (length expr))
       (or (symbol? (cadr expr))
           (and (pair? (cadr expr))
                (= 2 (length (cadr expr)))))))

(define (bard:compile-setter-form expr env)
  (let ((place-form (cadr expr)))
    (cond
     ((symbol? place-form) 
      (receive (i j)(find-variable-in-environment place-form env)
               (if (and i j)
                   ;; lexical variable
                   (bard:gen 'LSETR i j)
                   ;; global variable
                   (bard:gen 'GSETR place-form))))
     ((pair? place-form) (error "Setters for accessor functions are not yet implemented."))
     (else (error (str "Invalid place form in setter expression: "
                       place-form))))))

;;; ---------------------------------------------------------------------
;;; quotations
;;; ---------------------------------------------------------------------

(define (quote-form? expr) 
  (and (pair? expr)
       (= 2 (length expr))
       (eq? 'quote (car expr))))

(define (bard:compile-quote-form expr env) 
  (bard:gen 'CONST (cadr expr)))

(define (quasiquote-form? expr) 
  (and (pair? expr)
       (= 2 (length expr))
       (eq? 'quasiquote (car expr))))

;;; TODO: substitute the proper quasiquote
;;; from Bard 0.2.x
(define (bard:compile-quasiquote-form expr env) 
  (bard:gen 'CONST (cadr expr)))

;;; ---------------------------------------------------------------------
;;; application forms
;;; ---------------------------------------------------------------------

(define (bard:compile-list-expr expr env)
  (cond
   ((quote-form? expr)(bard:compile-quote-form expr env))
   ((quasiquote-form? expr)(bard:compile-quasiquote-form expr env))
   ((setter-form? expr)(bard:compile-setter-form expr env))
   ((special-form? expr)(bard:compile-special-form expr env))
   ((macro-form? expr)(bard:compile (bard:macroexpand expr) env))
   (else (bard:compile-funcall expr env))))

;;; ---------------------------------------------------------------------
;;; var refs
;;; ---------------------------------------------------------------------

(define (bard:compile-variable-reference expr env)
  (receive (i j)(find-variable-in-environment expr env)
           (if (and i j)
               ;; lexical variable
               (bard:gen 'LREF i j)
               ;; global variable
               (bard:gen 'GREF expr))))

;;; ---------------------------------------------------------------------
;;; literals
;;; ---------------------------------------------------------------------

(define (self-evaluating? expr)
  (or (null? expr)
      (boolean? expr)
      (number? expr)
      (char? expr)
      (string? expr)
      (procedure? expr)))

(define (bard:compile-self-evaluating expr)
  (bard:gen 'CONST expr))

;;; ---------------------------------------------------------------------
;;; the compiler
;;; ---------------------------------------------------------------------

(define (bard:compile expr env)
  (cond
   ((self-evaluating? expr)(bard:compile-self-evaluating expr))
   ((symbol? expr)(bard:compile-variable-reference expr env))
   (else (bard:compile-list-expr expr env))))

;;; ---------------------------------------------------------------------
;;; tests
;;; ---------------------------------------------------------------------

#| 

self-evaluating
---------------

(bard:compile '() '())
(bard:compile #t '())
(bard:compile 1 '())
(bard:compile #\A '())
(bard:compile "Foobar" '())

symbol
------

1. set up lexical env
(begin
  (define $env (null-env))
  (set! $env (environment-add-frame $env (make-environment-frame (list (make-binding 'a 1 mutable: #f)
                                                                       (make-binding 'b 2 mutable: #t)))))
  (set! $env (environment-add-frame $env (make-environment-frame (list (make-binding 'c 3 mutable: #f)
                                                                       (make-binding 'd 4 mutable: #t)))))
  (set! $env (environment-add-frame $env (make-environment-frame (list (make-binding 'e 5 mutable: #f)
                                                                       (make-binding 'f 6 mutable: #t))))))

2. compile variable references and setters

(bard:compile 'x $env)
(bard:compile 'a $env)
(bard:compile '(setter x) $env)
(bard:compile '(setter a) $env)

quotations
----------

(bard:compile '(quote x) $env)
(bard:compile '(quasiquote x) $env)

-------------
special forms
-------------

begin
-----

(bard:compile '(begin 1) $env)
(bard:compile '(begin 1 2 3) $env)
(bard:compile '(begin a c x) $env)

define
------

(bard:compile '(define function (add (x <fixnum>)(y <fixnum>)) (+ x y)) $env)
(bard:compile '(define protocol Rational ((numerator Ratio) Integer)((denominator Ratio) Integer)) $env)
(bard:compile '(define variable x 5) $env)


cond
----

(bard:compile '(cond (a 1)(b 2)) $env)


if
--

(bard:compile '(if a 1 0) $env)
(bard:compile '(if a (begin 1 #t) (begin 0 #f)) $env)

unless, when
------------

(bard:compile '(unless #t 1) $env)
(bard:compile '(when #t 1) $env)

|#
