;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          compiler1.lisp
;;;; Project:       Bard
;;;; Purpose:       the bard-to-bardo compiler
;;;; Author:        mikel evins
;;;; Copyright:     2014 mikel evins
;;;;
;;;; ***********************************************************************

;;; ---------------------------------------------------------------------
;;; ABOUT
;;; ---------------------------------------------------------------------
;;; Bardo is an s-expression-based intermediate language for Bard
;;; compiler1 compiles Bard source code to Bardo
;;; compiler2 compiles Bardo to Common Lisp
;;; compiler3 compiles the output of compiler2 to native code
;;; the .box interchange format stores Bardo or Common Lisp
;;; code for interchange among Bard sessions.

(in-package :bard)

;;; ---------------------------------------------------------------------
;;; compilation utilities
;;; ---------------------------------------------------------------------

(defun argument-count (expr n)
  (or (= n (length (cdr expr)))
      (error "Expected ~d arguments but found ~d" 
             n (length (cdr expr)))))

;;; ---------------------------------------------------------------------
;;; bard special forms
;;; ---------------------------------------------------------------------

(defparameter *bard-special-forms* (fset:wb-map :default (undefined)))

(defun define-special (name compiler)
  (setf *bard-special-forms*
        (fset:with *bard-special-forms* name compiler))
  name)

(defun special-form? (sym)
  (let ((compiler (fset:lookup *bard-special-forms* sym)))
    (if (defined? compiler)
        compiler
        nil)))

(defun compile-special-form (expr env)
  (let* ((sym (first expr))
         (compiler (fset:lookup *bard-special-forms* sym)))
    (if (defined? compiler)
        (funcall compiler expr env)
        (error "No such special form defined: ~s" sym))))


;;; ---------------------------------------------------------------------
;;; bard macros
;;; ---------------------------------------------------------------------

(defparameter *bard-macros* (fset:wb-map :default (undefined)))

(defun define-macro (name expander)
  (setf *bard-macros*
        (fset:with *bard-macros* name expander))
  name)

(defun bard-macro? (sym)
  (let ((expander (fset:lookup *bard-macros* sym)))
    (if (defined? expander)
        expander
        nil)))

(defun bard-macroexpand (expr)
  (let ((expander (fset:lookup *bard-macros* sym)))
    (if (defined? expander)
        (funcall expander expr)
        (error "No such macro defined: ~s" sym))))

;;; ---------------------------------------------------------------------
;;; expression compilers
;;; ---------------------------------------------------------------------

(defun compile-constant (expr)
  `(:constant ,expr))

(defun compile-variable (expr env)
  (if (find-variable expr env)
      `(:lexical-variable-ref ,expr)
      `(:global-variable-ref ,expr)))

(defun compile-application (expr env)
  (let ((op (first expr))
        (args (rest expr)))
    `(:apply ,(compile op env)
                   ,(mapcar (lambda (arg)(compile arg env))
                            args))))

;;; ---------------------------------------------------------------------
;;; main compiler entry point
;;; ---------------------------------------------------------------------

(defun compile (expr &optional (env (null-env)))
  (cond
    ;; named constants
    ((base-singleton? expr)(compile-constant expr))
    ((keywordp expr) (compile-constant expr))
    ;; variable references
    ((symbolp expr) (compile-variable expr env))
    ;; self-evaluating values
    ((atom expr) (compile-constant expr))
    ;; special forms
    ((special-form? (first expr)) (compile-special-form expr env))
    ;; macro forms
    ((bard-macro? (first expr)) (compile (bard-macroexpand expr) env))
    ;; procedure applications
    ((listp expr)(compile-application expr env))
    ;; unknown expression type
    (t (error "Unrecognized expression type: ~S" expr))))

