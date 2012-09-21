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



(define (special-form? expr)
  (table-ref $bard-special-forms (car expr) #f))

(define (bard:compile-special-form expr env val? more?)
  (let ((compiler (table-ref $bard-special-forms (car expr) #f)))
    (compiler expr env val? more?)))

(define (macro-form? expr)
  (table-ref $bard-macro-forms (car expr) #f))

(define (bard:macroexpand expr)
  (let ((expander (table-ref $bard-special-forms (car expr) #f)))
    (expander expr)))

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

(define (bard:compile-list-expr expr env val? more?)
  (cond
   ((special-form? expr)(bard:compile-special-form expr env val? more?))
   ((macro-form? expr)(bard:compile (bard:macroexpand expr) env val? more?))
   (t (bard:compile-funcall expr env val? more?))))

(define (bard:compile-variable-reference expr env val? more?)
  (receive (i j)(find-variable-in-environment expr env)
           (if (and i j)
               ;; lexical variable
               (bard:gen 'LREF i j)
               ;; global variable
               (bard:gen 'GREF expr))))

(define (bard:compile expr env val? more?)
  (cond
   ((self-evaluating? expr)(bard:compile-self-evaluating expr val? more?))
   ((symbol? expr)(bard:compile-variable-reference expr env val? more?))
   (else (bard:compile-list-expr expr env val? more?))))
