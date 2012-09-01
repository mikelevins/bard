;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          compile.scm
;;;; Project:       Bard
;;;; Purpose:       compiling special forms
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(define $special-forms-table (make-table test: eq?))

(define (%defspecial nm eval-fn)
  (table-set! $special-forms-table nm eval-fn))

(define (%special-compiler nm)
  (table-ref $special-forms-table nm #f))

(define (%special-form? expr)
  (and (table-ref $special-forms-table (car expr) #f)
           #t))

(define (%compile-special-form expr env)
  (let* ((op (car expr))
         (compiler (table-ref $special-forms-table op #f)))
    (if compiler
        (compiler (cdr expr) env)
        (error (string-append "unrecognized special form" (object->string (car expr)))))))

;;; ----------------------------------------------------------------------
;;; special forms defined
;;; ----------------------------------------------------------------------

(define (%special-not-yet-implemented . args)
  (error "Special-form compiler not yet implemented"))

;;; begin
;;; ----------------------------------------------------------------------

(define %compile-begin
  (lambda (expr env) 
    (cond
     ((null? expr)(%gen 'NOTHING))
     ((= 1 (length expr))(%compile (car expr) env))
     (else (interpose (%gen 'POP)
                      (map (lambda (x)(%compile x env))
                           expr))))))

(%defspecial 'begin %compile-begin)

;;; (define $env (env-add-frame (null-env) (make-env-frame (list (make-binding 'x 5)))))
;;; (%compile '(begin 2 3 x) $env)

;;; cond
;;; ----------------------------------------------------------------------

(%defspecial 'cond %special-not-yet-implemented)

;;; define
;;; ----------------------------------------------------------------------

(%defspecial 'define %special-not-yet-implemented)

;;; define-macro prototype & body
;;; ----------------------------------------------------------------------

(%defspecial 'define-macro %special-not-yet-implemented)

;;; define-function
;;; ----------------------------------------------------------------------

(%defspecial 'define-function %special-not-yet-implemented)

;;; function
;;; ----------------------------------------------------------------------

(%defspecial 'function %special-not-yet-implemented)

;;; if
;;; ----------------------------------------------------------------------

(%defspecial 'if %special-not-yet-implemented)

;;; method
;;; ----------------------------------------------------------------------

(%defspecial 'method %special-not-yet-implemented)

;;; quasiquote
;;; ----------------------------------------------------------------------
;;; after norvig

(define (constant? exp)
  (if (pair? exp)
      (eq? (car exp) 'quote)
      (not (symbol? exp))))

(define (combine-skeletons left right exp)
  (cond
   ((and (constant? left) (constant? right)) 
    (if (and (eqv? (%eval left) (car exp))
             (eqv? (%eval right) (cdr exp)))
        (list 'quote exp)
        (list 'quote (cons (%eval left) (%eval right)))))
   ((null? right) (list 'list left))
   ((and (pair? right) (eq? (car right) 'list))
    (cons 'list (cons left (cdr right))))
   (else (list 'add-first left right))))

(define (%expand-quasiquote exp nesting)
      (cond
       ((not (pair? exp)) 
	(if (constant? exp) exp (list 'quote exp)))
       ((and (eq? (car exp) 'unquote) (= (length exp) 2))
	(if (= nesting 0)
	    (%cadr exp)
	    (combine-skeletons ''unquote 
			       (%expand-quasiquote (cdr exp) (- nesting 1))
			       exp)))
       ((and (eq? (car exp) 'quasiquote) (= (length exp) 2))
	(combine-skeletons ''quasiquote 
			   (%expand-quasiquote (cdr exp) (+ nesting 1))
			   exp))
       ((and (pair? (car exp))
	     (eq? (caar exp) 'unquote-splicing)
	     (= (length (car exp)) 2))
	(if (= nesting 0)
	    (list 'append (%cadr (car exp))
		  (%expand-quasiquote (cdr exp) nesting))
	    (combine-skeletons (%expand-quasiquote (car exp) (- nesting 1))
			       (%expand-quasiquote (cdr exp) nesting)
			       exp)))
       (else (combine-skeletons (%expand-quasiquote (car exp) nesting)
				(%expand-quasiquote (cdr exp) nesting)
				exp))))


(%defspecial 'quasiquote 
             (lambda (expr env)
               (%compile (%expand-quasiquote (%cadr expr) 0) env)))

(%defspecial 'unquote (lambda (expr env) (error "invalid context for unquote")))
(%defspecial 'unquote-splicing (lambda (expr env) (error "invalid context for unquote-splicing")))

;;; quote
;;; ----------------------------------------------------------------------

(%defspecial 'quote %special-not-yet-implemented)

;;; setter
;;; ----------------------------------------------------------------------

(%defspecial 'setter 
             (lambda (expr env)
               (let ((var (car expr)))
                 (if (symbol? var)
                     (receive (i j) (find-in-env var env)
                              (if (and i j)
                                  (%gen 'LSETTER i j)
                                  (%gen 'MSETTER var)))
                     (let ((varname (car var))
                           (obj-expr (cadr var)))
                       (%seq 
                        (%compile obj-expr env)
                        (%gen 'SSETTER varname)))))))

;;; (define $env (env-add-frame (null-env) (make-env-frame (list (make-binding 'x 5)))))
;;; (%compile '(setter x) $env)
;;; (%compile '(setter y) $env)

;;; time
;;; ----------------------------------------------------------------------

(%defspecial 'time %special-not-yet-implemented)
