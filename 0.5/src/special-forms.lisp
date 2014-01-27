;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          special-forms.lisp
;;;; Project:       Bard
;;;; Purpose:       special-form definitions for compiler1
;;;; Author:        mikel evins
;;;; Copyright:     2014 mikel evins
;;;;
;;;; ***********************************************************************


(in-package :bard)


;;; ---------------------------------------------------------------------
;;; special form definitions
;;; ---------------------------------------------------------------------

;;; abort
;;; ---------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel)
  (define-special '|abort|
      (lambda (expr env)
        (argument-count expr 1)
        (let* ((condition (compile (cadr expr) env)))
          `(:abort ,condition)))))

;;; and
;;; ---------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel)
  (define-special '|and|
      (lambda (expr env)
        (let ((exprs (cdr expr)))
          (if (null exprs)
              (compile (true) env)
              (let ((test (compile (car exprs) env))
                    (more (cdr exprs)))
                (if more
                    `(:if ,test ,(compile (cons '|and| more) env) ,(compile (false) env))
                    test)))))))

;;; begin
;;; ---------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel)
  (define-special '|begin|
      (lambda (expr env)
        (let* ((exprs (rest expr))
               (body (mapcar (lambda (ex)(compile ex env))
                             exprs)))
          `(:begin ,@body)))))

;;; case
;;; ---------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel)
  (define-special '|case|
      (lambda (expr env)
        (let* ((case-val (compile (second expr) env))
               (clause-forms (nthcdr 2 expr))
               (clauses (mapcar (lambda (clause)
                                  (let* ((test (first clause))
                                         (body (rest clause)))
                                    (list test (compile (cons '|begin| body) env))))
                                clause-forms)))
          `(:case ,case-val ,@clauses)))))


;;; cond
;;; ---------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel)
  (define-special '|cond|
      (lambda (expr env)
        (let* ((clause-forms (cdr expr))
               (clauses (mapcar (lambda (clause)
                                  (let* ((test (first clause))
                                         (body (rest clause)))
                                    (list (compile test env) 
                                          (compile (cons '|begin| body) env))))
                                clause-forms)))
          `(:cond ,@clauses)))))


;;; catch
;;; ---------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel)
  (define-special '|catch|
      (lambda (expr env)
        (let ((tag (cadr expr))
              (body (compile (cons '|begin| (cddr expr)) env)))
          `(:catch ,tag ,body)))))

;;; quote
;;; ---------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel)
  (define-special '|quote|
      (lambda (expr env)
        (argument-count expr 1)
        `(:constant ,(cadr expr)))))

;;; throw
;;; ---------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel)
  (define-special '|throw|
      (lambda (expr env)
        (let ((tag (cadr expr))
              (val (caddr expr)))
          `(:throw ,tag val)))))

