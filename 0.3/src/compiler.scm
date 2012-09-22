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

(define (bard:gen-label)
  `(label ,(gensym)))

(define (bard:gen . args)
  args)

;;; ---------------------------------------------------------------------
;;; special forms
;;; ---------------------------------------------------------------------

(define (bard:compile-begin args env val? more?)
  (let loop ((args args)
             (result '()))
    (if (null? args)
        (reverse result)
        (if (null? (cdr args))
            (loop (cdr args)
                  (cons (bard:compile (car args) env #t more?)
                        result))
            (loop (cdr args)
                  (cons (bard:gen 'POP)
                        (cons (bard:compile (car args) env #t more?)
                              result)))))))

(define (bard:compile-cond expr env val? more?) 
  #f)

(define (bard:compile-define-function expr env val? more?) 
  #f)

(define (bard:compile-define-macro expr env val? more?) 
  #f)

(define (bard:compile-define-protocol expr env val? more?) 
  #f)

(define (bard:compile-define-schema expr env val? more?) 
  #f)

(define (bard:compile-define-variable expr env val? more?) 
  #f)

(define (bard:compile-define-vector expr env val? more?) 
  #f)

(define (bard:compile-define expr env val? more?) 
  (let ((op (car expr)))
    (case op
      ((function)(bard:compile-define-function (cdr expr) env val? more?))
      ((macro)(bard:compile-define-macro (cdr expr) env val? more?))
      ((protocol)(bard:compile-define-protocol (cdr expr) env val? more?))
      ((schema)(bard:compile-define-schema (cdr expr) env val? more?))
      ((variable)(bard:compile-define-variable (cdr expr) env val? more?))
      ((vector)(bard:compile-define-vector (cdr expr) env val? more?))
      (else (error (str "Unrecognized definition type: define " op))))))

(define (bard:compile-let expr env val? more?) 
  #f)

(define (bard:compile-loop expr env val? more?) 
  #f)

(define (bard:compile-match expr env val? more?) 
  #f)

(define (bard:compile-method expr env val? more?) 
  #f)

(define (bard:compile-unless expr env val? more?) 
  (let ((test (bard:compile (car expr) env #t more?))
        (body (bard:compile-begin (cdr expr) env val? more?))
        (continue-before (bard:gen-label))
        (continue-after (bard:gen-label)))
    (append
     (list test)
     (list (bard:gen 'FJUMP continue-before))
     (list (bard:gen 'JUMP continue-after))
     (list continue-before)
     body
     (list continue-after))))

(define (bard:compile-when expr env val? more?) 
  (let ((test (bard:compile (car expr) env #t more?))
        (body (bard:compile-begin (cdr expr) env val? more?))
        (continue-before (bard:gen-label))
        (continue-after (bard:gen-label)))
    (append
     (list test)
     (list (bard:gen 'TJUMP continue-before))
     (list (bard:gen 'JUMP continue-after))
     (list continue-before)
     body
     (list continue-after))))

(define $bard-special-forms 
  (list->table
   `((begin . ,(lambda (expr env val? more?)(bard:compile-begin (cdr expr) env val? more?)))
     (cond . ,(lambda (expr env val? more?) (bard:compile-cond (cdr expr) env val? more?)))
     (define . ,(lambda (expr env val? more?) (bard:compile-define (cdr expr) env val? more?)))
     (let . ,(lambda (expr env val? more?) (bard:compile-let (cdr expr) env val? more?)))
     (loop . ,(lambda (expr env val? more?) (bard:compile-loop (cdr expr) env val? more?)))
     (match . ,(lambda (expr env val? more?) (bard:compile-match (cdr expr) env val? more?)))
     (method . ,(lambda (expr env val? more?) (bard:compile-method (cdr expr) env val? more?)))
     (unless . ,(lambda (expr env val? more?) (bard:compile-unless (cdr expr) env val? more?)))
     (when . ,(lambda (expr env val? more?) (bard:compile-when (cdr expr) env val? more?))))))

(define (special-form? expr)
  (table-ref $bard-special-forms (car expr) #f))

(define (bard:compile-special-form expr env val? more?)
  (let ((compiler (table-ref $bard-special-forms (car expr) #f)))
    (compiler expr env val? more?)))

;;; ---------------------------------------------------------------------
;;; macros
;;; ---------------------------------------------------------------------

(define (macro-form? expr)
  (table-ref $bard-macro-forms (car expr) #f))

(define (bard:macroexpand expr)
  (let ((expander (table-ref $bard-special-forms (car expr) #f)))
    (expander expr)))

;;; ---------------------------------------------------------------------
;;; primitives
;;; ---------------------------------------------------------------------

(define (bard:compile-primitive-call expr env val? more?)
  (let ((op (car expr))
        (args (cdr expr)))
    (if (or (primitive-side-effects? op)
            val?)
        ;; the return value is needed, or the primitive has side effects
        (bard:code-sequence
         ;; compile args, generating code to push the values
         (bard:compile-list args env)
         (bard:gen 'PRIM (primitive-opfn op))
         (if (not val?)(bard:gen 'POP))
         (if (not more?)(bard:gen 'RETURN)))
        ;; the return value is not needed and the primitive has no side effects
        (bard:compile-begin args env val? more?))))

;;; ---------------------------------------------------------------------
;;; funcalls
;;; ---------------------------------------------------------------------

(define (bard:compile-function-call expr env val? more?)
  (let ((op (car expr))
        (args (cdr expr)))
    (if more?
        ;; save the continuation because the result will be used for more computation
        (let ((continue (bard:gen-label 'continue)))
          (bard:code-sequence
           (bard:gen 'SAVE continue)
           (bard:compile-list args env)
           (bard:compile op env)
           (bard:gen 'CALLJ (length args))
           (list continue)
           (if (not val?)(bard:gen 'POP))))
        ;; tail call, so no need to save the continuation
        (bard:code-sequence
         (bard:compile-list args env)
         (bard:compile op env)
         (bard:gen 'CALLJ (length args))))))

(define (bard:compile-funcall expr env val? more?)
  (if (primitive? (car expr))
      (bard:compile-primitive-call expr env val? more?)
      (bard:compile-function-call expr env val? more?)))

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

(define (bard:compile-setter-form expr env val? more?)
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
;;; application forms
;;; ---------------------------------------------------------------------

(define (bard:compile-list-expr expr env val? more?)
  (cond
   ((setter-form? expr)(bard:compile-setter-form expr env val? more?))
   ((special-form? expr)(bard:compile-special-form expr env val? more?))
   ((macro-form? expr)(bard:compile (bard:macroexpand expr) env val? more?))
   (t (bard:compile-funcall expr env val? more?))))

;;; ---------------------------------------------------------------------
;;; var refs
;;; ---------------------------------------------------------------------

(define (bard:compile-variable-reference expr env val? more?)
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
      (string? expr)))

(define (bard:compile-self-evaluating expr val? more?)
  (if val?
      (bard:gen 'CONST expr)
      (list
       (bard:gen 'CONST expr)
       (bard:gen 'POP))))

;;; ---------------------------------------------------------------------
;;; the compiler
;;; ---------------------------------------------------------------------

(define (bard:compile expr env val? more?)
  (cond
   ((self-evaluating? expr)(bard:compile-self-evaluating expr val? more?))
   ((symbol? expr)(bard:compile-variable-reference expr env val? more?))
   (else (bard:compile-list-expr expr env val? more?))))

;;; ---------------------------------------------------------------------
;;; tests
;;; ---------------------------------------------------------------------

#| 

self-evaluating
(bard:compile '() '() #t #f)
(bard:compile #t '() #t #f)
(bard:compile 1 '() #t #f)
(bard:compile #\A '() #t #f)
(bard:compile "Foobar" '() #t #f)

symbol

1. set up lexical env
(define $env (null-env))
(set! $env
      (environment-add-frame $env
                             (make-environment-frame (list (make-binding 'a 1 mutable: #f)
                                                           (make-binding 'b 2 mutable: #t)))))
(set! $env
      (environment-add-frame $env
                             (make-environment-frame (list (make-binding 'c 3 mutable: #f)
                                                           (make-binding 'd 4 mutable: #t)))))
(set! $env
      (environment-add-frame $env
                             (make-environment-frame (list (make-binding 'e 5 mutable: #f)
                                                           (make-binding 'f 6 mutable: #t)))))

2. compile variable references
(bard:compile 'x $env #t #f)
(bard:compile 'a $env #t #f)
(bard:compile '(setter x) $env #t #f)
(bard:compile '(setter a) $env #t #f)

3. compile begin
(bard:compile '(begin 1) $env #t #f)
(bard:compile '(begin 1 2 3) $env #t #f)
(bard:compile '(begin a c x) $env #t #f)

4. compile unless
(bard:compile '(unless #t 1) $env #t #f)

|#
