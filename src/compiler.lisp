;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: asdf -*-
;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          compiler.lisp
;;;; Project:       Bard - a modern Lisp
;;;; Purpose:       compiler
;;;; Author:        mikel evins
;;;; Copyright:     2010 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard)

;;; ============================================================
;;; BARD compiler
;;; ============================================================

(defmethod sequence-compiler (x exp)
  (error "unrecognized sequence expression-type: ~A" exp))

(defmethod sequence-compiler ((x symbol) exp)
  (lambda (expression env)
    "not yet implemented: compiler for symbols in function position"))

(defmethod sequence-compiler ((x fset:seq) exp)
  (lambda (expression env)
    "not yet implemented: compiler for sequence expressions in function position"))

(defmethod sequence-compiler ((x fset:map) exp)
  (lambda (expression env) 
    (get-key (element expression 0) (compile (element expression 1) env))))

(defun compile-variable-reference (exp env)
  (cond
    ((unqualified-symbol? exp)
     (lambda (expression env)
       (lookup-variable (find-symbol-in-current-module exp env) env)))
    ((module-qualified-symbol? exp) 
     (lambda (expression env)
       (lookup-variable (module-qualified-symbol exp) env)))
    (t (error "unrecognized symbol syntax: ~a" exp))))

(defun compile-sequence-expression (exp env)
  (let ((compiler (sequence-compiler (element exp 0) exp)))
    (funcall compiler exp env)))

(defmethod expression-compiler ((exp symbol))
  (lambda (exp env) (compile-variable-reference exp env)))

(defmethod expression-compiler ((exp fset:seq))
  (lambda (exp env) (compile-sequence-expression exp env)))

(defun compile (exp env)
  (if (self-evaluating? exp)
      exp
      (funcall (expression-compiler exp) exp env)))

#| Testing

;;; simple expressions

(bard:compile (bard:read "void") nil)
(bard:compile (bard:read "2") nil)
(bard:compile (bard:read "123456789.123456789") nil)
(bard:compile (bard:read "\\A") nil)
(bard:compile (bard:read "\\space") nil)
(bard:compile (bard:read "Name:") nil)
(bard:compile (bard:read "Sym") nil)
(bard:compile (bard:read "true") nil)
(bard:compile (bard:read "false") nil)
(bard:compile (bard:read "(Name: , \"Barney\")") nil)
(bard:compile (bard:read "(0 1 2 3)") nil)
(bard:compile (bard:read "[0 1 2 3]") nil)
(bard:compile (bard:read "{ Name: \"Fred\", Age: 105}") nil)

;;; variable references
(bard:compile (bard:read "foo") nil)
(bard:compile (bard:read "user.foo") nil)

;;; function calls, macros, and special forms
(bard:compile (bard:read "(+ 2 3)") nil)
(bard:compile (bard:read "((method (x y) (* x y)) 2 3)") nil)
(bard:compile (bard:read "({name: \"Fred\", age: 101} name:)") nil)
(bard:compile (bard:read "({name: \"Fred\", age: 101} age:)") nil)
(bard:compile (bard:read "({name: \"Fred\", age: 101} missing:)") nil)


|#