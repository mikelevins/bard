;;;; ***********************************************************************
;;;;
;;;; Name:          compiler-define.scm
;;;; Project:       Bard
;;;; Purpose:       compilation of define forms
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(declare (extended-bindings))

;;; ---------------------------------------------------------------------
;;; 
;;; ---------------------------------------------------------------------

;;; (define (foo bar baz) expr1 expr2 ...)
(define (bard:compile-define-function expr env)
  (let* ((proto (cadr expr))
         (fname (car proto))
         (params (cdr proto))
         (body (drop 2 expr)))
    `(DEF ,fname
          (FN ,params ,(bard:compile
                        (cons 'begin body))))))

;;; (define foo bar)
(define (bard:compile-define-variable expr env)
  (let ((var (cadr expr))
        (val (bard:compile (caddr expr) env)))
    `(DEF ,var ,val)))

(define (bard:compile-define expr env)
  (let ((discrim (cadr expr)))
    (case discrim
      ;; TODO: insert special keyword handling here for such forms as
      ;; define record and define protocol
      (else ;; no special keyword after define, so either a variable
       ;; or a function definition
       (if (list? discrim)
           (bard:compile-define-function expr env)
           (if (symbol? discrim)
               (bard:compile-define-variable expr env)
               (error "Unrecognized define form:" expr)))))))



