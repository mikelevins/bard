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

(define (%special-compiler nm)
  (table-ref $special-forms-table nm #f))

(define (setter-form? expr)
  (and (list? (car expr))
       (eq? 'setter (car (car expr)))))

(define (%special-form? expr)
  (or (setter-form? expr)
      (and (table-ref $special-forms-table (%car expr) #f)
           #t)))


(define (%compile-special-form expr env)
  (if (setter-form? expr)
      (%compile-setter-form expr env)
      (let* ((op (%car expr))
             (compiler (table-ref $special-forms-table op #f)))
        (if compiler
            (compiler expr env)
            (error (string-append "unrecognized special form" (%as-string (%car expr))))))))


;;; ----------------------------------------------------------------------
;;; special forms defined
;;; ----------------------------------------------------------------------

(define (%special-not-yet-implemented . args)
  (error "Special-form compiler not yet implemented"))

;;; begin
;;; ----------------------------------------------------------------------

(%defspecial 'begin
             (lambda (expr env) 
               (cond
                ((null? expr)(%gen NOTHING))
                ((= 1 (length expr))(%compile (car expr) env))
                (else (%seq (%compile (car expr) env)
                            (%gen POP)
                            (%compile-begin (cdr expr) env))))))

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
	    (list 'append (%cadr (%car exp))
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

;;; time
;;; ----------------------------------------------------------------------

(%defspecial 'time %special-not-yet-implemented)




