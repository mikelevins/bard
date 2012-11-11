;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          eval.scm
;;;; Project:       Bard
;;;; Purpose:       the bard interpreter
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

;;; ---------------------------------------------------------------------
;;; ABOUT
;;; ---------------------------------------------------------------------
;;; the execution model is this:
;;; 1. compile an expression in the current environment,
;;;    yielding a list of primitive forms ("instructions')
;;; 2. execute the list of instructions, yielding a set 
;;;    of outputs
;;;
;;; an instruction is a form like:
;;; (OP arg1 arg2 ...)
;;;
;;; OP is a symbol that identifies an execution procedure.
;;; loading replaces each OP with the execution
;;; procedure it refers to. %exec applies each execution
;;; procedure to its arguments.
;;;
;;; a list of instructions can be converted to serialized 
;;; form for storage, and can be reconstituted from
;;; storage by deserialization, which enables it to be
;;; executed. That's how we compile files: by compiling
;;; the expressions they contain and serializing the
;;; results to an object file.
;;;
;;; we can also serialize the toplevel environment.
;;; serializing the environment (including modules and
;;; execution stack) yields a bard image that can be
;;; loaded and resumed.

;;; ---------------------------------------------------------------------
;;; self-evaluating expressions
;;; ---------------------------------------------------------------------

(define (%self-evaluating? expr)
  (or (null? expr)
      (and (not (pair? expr))
           (not (symbol? expr)))))

(define (%compile-self-evaluating expr env) 
  `((CONST ,expr)))

;;; ---------------------------------------------------------------------
;;; special forms
;;; ---------------------------------------------------------------------

(define $special-forms-table (make-table test: eq?))

(define (%define-special-form form-name compiler-fn)
  (table-set! $special-forms-table form-name compiler-fn))

(define (%special-form? form)
  (and (pair? form)
       (table-ref $special-forms-table (car form) #f)))

(define (%compile-lambda expr env)
  (let ((params (cadr expr))
        (body (cddr expr)))
    `((METHOD ,params ,body ,env))))

(define (%compile-sequence exprs env)
  `((SEQ ,@(map (lambda (e)(%compile-expression e env)) exprs))))

(define (%compile-begin expr env)
  (%compile-sequence (cdr expr) env))

(define (%compile-cond expr env)
  (let* ((clauses (cdr expr))
         (compiled-clauses (map (lambda (c)
                                  (list (if (eq? (car c) 'else)
                                            (%compile-expression #t env)
                                            (%compile-expression (car c) env))
                                        (%compile-sequence (cdr c) env)))
                                clauses)))
    `((COND ,@compiled-clauses))))

(define (%compile-define-class expr env) #f)
(define (%compile-define-macro expr env) #f)
(define (%compile-define-method expr env) #f)
(define (%compile-define-protocol expr env) #f)
(define (%compile-define-record expr env) #f)
(define (%compile-define-variable expr env) #f)
(define (%compile-define-vector expr env) #f)

(define (%compile-define expr env)
  (let ((key (cadr expr)))
    (case key
      ;; define class
      ((class) (%compile-define-class expr env))
      ;; define macro
      ((macro) (%compile-define-macro expr env))
      ;; define method
      ((method) (%compile-define-method expr env))
      ;; define protocol
      ((protocol) (%compile-define-protocol expr env))
      ;; define record
      ((record) (%compile-define-record expr env))
      ;; define variable
      ((variable) (%compile-define-variable expr env))
      ;; define vector
      ((vector) (%compile-define-vector expr env))
      (else (error (str "Unrecognized defining form: " key))))))

(define (%compile-ensure) #f)

(define (%compile-if expr env) 
  (let ((test (list-ref expr 1))
        (consequent (list-ref expr 2))
        (alternate (if (> (length expr) 3)
                       (list-ref expr 3)
                       (%nothing))))
    (%compile-cond `(cond (,test ,consequent)(else ,alternate)) env)))

(define (%compile-let) #f)
(define (%compile-loop) #f)
(define (%compile-macroexpand) #f)
(define (%compile-match) #f)
(define (%compile-quasiquote) #f)

(define (%compile-quote expr env)
  `((CONST ,(cadr expr))))

(define (%compile-unless expr env) 
  (let ((test (list-ref expr 1))
        (consequent (%nothing))
        (alternate (cddr expr)))
    (%compile-cond `(cond (,test ,@consequent)(else ,@alternate)) env)))

(define (%compile-unquote) #f)
(define (%compile-unquote-splicing) #f)

(define (%compile-when expr env) 
  (let ((test (list-ref expr 1))
        (consequent (cddr expr))
        (alternate (%nothing)))
    (%compile-cond `(cond (,test ,@consequent)(else ,@alternate)) env)))

(define (%compile-with-exit) #f)

(%define-special-form 'λ %compile-lambda)
(%define-special-form '^ %compile-lambda)
(%define-special-form 'lambda %compile-lambda)
(%define-special-form 'method %compile-lambda)
(%define-special-form 'begin %compile-begin)
(%define-special-form 'cond %compile-cond)
(%define-special-form 'define %compile-define)
(%define-special-form 'ensure %compile-ensure)
(%define-special-form 'if %compile-if)
(%define-special-form 'let %compile-let)
(%define-special-form 'loop %compile-loop)
(%define-special-form 'match %compile-match)
(%define-special-form 'quasiquote %compile-quasiquote)
(%define-special-form 'quote %compile-quote)
(%define-special-form 'unless %compile-unless)
(%define-special-form 'unquote %compile-unquote)
(%define-special-form 'unquote-splicing %compile-unquote-splicing)
(%define-special-form 'when %compile-when)
(%define-special-form 'with-exit %compile-with-exit)

(define (%compile-special-form expr env)
  (let ((comp (table-ref $special-forms-table (car expr))))
    (comp expr env)))

;;; ---------------------------------------------------------------------
;;; macros
;;; ---------------------------------------------------------------------

(define $macro-forms-table (make-table test: eq?))

(define (%define-macro-form form-name expander-fn)
  (table-set! $macro-forms-table form-name expander-fn))

(define (%macro-form? form)
  (and (pair? form)
       (table-ref $macro-forms-table (car form) #f)))


;;; ---------------------------------------------------------------------
;;; app forms
;;; ---------------------------------------------------------------------

;;; ---------------------------------------------------------------------
;;; execution of compiled expressions
;;; ---------------------------------------------------------------------

(define (%exec expr env)
  #f)

;;; ---------------------------------------------------------------------
;;; evaluation
;;; ---------------------------------------------------------------------

(define (%compile-expression expr #!optional (env '()))
  (if (%self-evaluating? expr)
      (%compile-self-evaluating expr env)
      (cond
       ((%special-form? expr)(%compile-special-form expr env))
       ((%macro-form? expr)(%compile-expression (%macroexpand expr) env))
       (else (%compile-app-form expr env)))))

(define (%eval expr #!optional (env '()))
  (%exec (%compile-expression expr env)))

;;; (define $env '())
;;; (%compile-expression '() $env)
;;; (%compile-expression 1.2 $env)
;;; (%compile-expression #f $env)
;;; (%compile-expression "Foo" $env)
;;; (%compile-expression '(^ (x) (* x x)) $env)
;;; (%compile-expression '(begin 1 2 3) $env)
;;; (%compile-expression '(cond (1 1)(2 2)) $env)
;;; (%compile-expression '(if 1 1 2) $env)
;;; (%compile-expression '(quote x) $env)
;;; (%compile-expression '(unless 1 1 2) $env)
;;; (%compile-expression '(when 1 1 2) $env)
