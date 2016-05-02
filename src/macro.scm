;;;; ***********************************************************************
;;;;
;;;; Name:          macro.scm
;;;; Project:       Bard
;;;; Purpose:       bard macros
;;;; Author:        mikel evins
;;;; Copyright:     2016 by mikel evins
;;;;
;;;; ***********************************************************************

(declare (extended-bindings))

;;; a macro-expander is a function of the form:
;;; (lambda (expr env) ...) => sexpr

(define $bard-macro-functions (make-table test: eq?))

(define (%define-macro-function name mfun)
  (table-set! $bard-macro-functions name mfun))

(define (%macro-form? expr)
  (and (list? expr)
       (not (null? expr))
       (table-ref $bard-macro-functions (car expr) #f)
       #t))

(define (%macroexpand expr)
  (let* ((expander (table-ref $bard-macro-functions (car expr) #f)))
    (if expander
        (%funcall expander expr)
        (error "undefined macro in expression" expr))))

(define (%eval-macro-form expr env)
  (%eval (%macroexpand expr) env))

;;; ---------------------------------------------------------------------
;;; standard macros
;;; ---------------------------------------------------------------------

(%define-macro-function 'and
                        (lambda (expr)
                          (let ((var (gensym)))
                            (if (null? (cdr expr))
                                #t
                                (if (null? (cddr expr))
                                    `(let ((,var ,(cadr expr))) (if ,var ,var #f))
                                    `(let ((,var ,(cadr expr))) (if ,var (and ,@(cddr expr)) #f)))))))

(%define-macro-function 'or
                        (lambda (expr)
                          (let ((var (gensym)))
                            (if (null? (cdr expr))
                                #f
                                (if (null? (cddr expr))
                                    `(let ((,var ,(cadr expr))) (if ,var ,var #f))
                                    `(let ((,var ,(cadr expr))) (if ,var ,var (or ,@(cddr expr)))))))))
