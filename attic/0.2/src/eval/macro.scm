;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          macro.scm
;;;; Project:       Bard
;;;; Purpose:       bard macros
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

;;; a macro-expander is a function of the form:
;;; (lambda (expr env) ...) => sexpr

(define $bard-macro-functions (make-table test: eq?))

(define (%define-macro-function name mfun)
  (table-set! $bard-macro-functions name mfun))

(define (%macro-form? expr)
  (and (%list? expr)
       (not (%null? expr))
       (table-ref $bard-macro-functions (%car expr) #f)
       #t))

(define (%macroexpand expr)
  (let* ((expander (table-ref $bard-macro-functions (%car expr) #f)))
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
                            (if (%null? (%cdr expr))
                                (%true)
                                (if (%null? (%cddr expr))
                                    `(let ((,var ,(%cadr expr))) (if ,var ,var ,(%false)))
                                    `(let ((,var ,(%cadr expr))) (if ,var (and ,@(%cddr expr)) ,(%false))))))))

(%define-macro-function 'or
                        (lambda (expr)
                          (let ((var (gensym)))
                            (if (%null? (%cdr expr))
                                (%false)
                                (if (%null? (%cddr expr))
                                    `(let ((,var ,(%cadr expr))) (if ,var ,var ,(%false)))
                                    `(let ((,var ,(%cadr expr))) (if ,var ,var (or ,@(%cddr expr)))))))))
