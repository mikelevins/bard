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
  (declare (ignore x))
  (error "unrecognized sequence expression-type: ~A" exp))

(defmethod sequence-compiler ((x symbol) exp)
  (cond
    ((eql (name x) 'bard::|quote|) (lambda (expression env) (element expression 1)))
    (t (lambda (expression env) 
         ))))

(defmethod sequence-compiler ((x fset:seq) exp)
  (declare (ignore x exp))
  (lambda (expression env)
    (declare (ignore expression env))
    "not yet implemented: compiler for sequence expressions in function position"))

(defmethod sequence-compiler ((x fset:map) exp)
  (declare (ignore x exp))
  (lambda (expression env) 
    (apply (compile (element expression 0) env) 
           (map-over (lambda (y)(compile y env))
                     (fset:less-first expression)))))

(defun compile-sequence-expression (exp env)
  (let ((compiler (sequence-compiler (element exp 0) exp)))
    (funcall compiler exp env)))

(defmethod expression-compiler ((exp symbol))
  (cond
    ((unqualified-symbol? exp)
     (lambda (expression env)
       (lookup-variable (find-symbol-in-current-module expression env) env)))
    ((module-qualified-symbol? exp) 
     (lambda (expression env)
       (lookup-variable (module-qualified-symbol expression) env)))
    (t (error "unrecognized symbol syntax: ~a" exp))))

(defmethod expression-compiler ((exp fset:seq))
  (lambda (exp env) (compile-sequence-expression exp env)))

(defun compile (exp env)
  (if (self-evaluating? exp)
      exp
      (funcall (expression-compiler exp) exp env)))

#| Testing

;;; set up the environment
(bard::init-modules)

;;; simple expressions

(bard:compile (bard:read "nothing") (bard::bard-toplevel-environment))
(bard:compile (bard:read "2") (bard::bard-toplevel-environment))
(bard:compile (bard:read "123456789.123456789") (bard::bard-toplevel-environment))
(bard:compile (bard:read "\\A") (bard::bard-toplevel-environment))
(bard:compile (bard:read "\\space") (bard::bard-toplevel-environment))
(bard:compile (bard:read "Name:") (bard::bard-toplevel-environment))
(bard:compile (bard:read "bard.*module*") (bard::bard-toplevel-environment))
(bard:compile (bard:read "true") (bard::bard-toplevel-environment))
(bard:compile (bard:read "false") (bard::bard-toplevel-environment))
(bard:compile (bard:read "(Name: , \"Barney\")") (bard::bard-toplevel-environment))
(bard:compile (bard:read "(0 1 2 3)") (bard::bard-toplevel-environment))
(bard:compile (bard:read "[0 1 2 3]") (bard::bard-toplevel-environment))
(bard:compile (bard:read "{ Name: \"Fred\", Age: 105}") (bard::bard-toplevel-environment))

;;; variable references
(bard:compile (bard:read "foo") (bard::bard-toplevel-environment))
(bard:compile (bard:read "user.foo") (bard::bard-toplevel-environment))

;;; function calls, macros, and special forms
(bard:compile (bard:read "(quote (+ 2 3))") (bard::bard-toplevel-environment))
(bard:compile (bard:read "(+ 2 3)") (bard::bard-toplevel-environment))
(bard:compile (bard:read "((method (x y) (* x y)) 2 3)") (bard::bard-toplevel-environment))
(bard:compile (bard:read "({name: \"Fred\", age: 101} name:)") (bard::bard-toplevel-environment))
(bard:compile (bard:read "({name: \"Fred\", age: 101} age:)") (bard::bard-toplevel-environment))
(bard:compile (bard:read "({name: \"Fred\", age: 101} missing:)") (bard::bard-toplevel-environment))

|#

(in-package :cl-user)

(defparameter $test-exps
  (list
   ;; literal atoms
   "nothing"
   "2"
   "123456789.123456789"
   "\\A"
   "\\space"
   "Name:"
   "bard.*module*"
   "true"
   "false"
   ;; literal <Pair>
   "(Name: , \"Barney\")"
   ;; literal <Map>
   "{ Name: \"Fred\", Age: 105}"
   ;; <Sequence>
   "(quote 2)"
   "(quote (+ 2 3))"
   "(+ 2 3)"
   "((method (x y) (* x y)) 2 3)"
   "({name: \"Fred\", age: 101} name:)"
   "({name: \"Fred\", age: 101} age:)"
   "({name: \"Fred\", age: 101} missing:)"))

(defun run-compiler-tests ()
  (bard::init-modules)
  (let ((env (bard::bard-toplevel-environment)))
    (flet ((comp (x e) 
             (handler-case (bard:compile (bard:read x) e)
               (error (err) (format nil "error: ~S" err)))))
      (dolist (x $test-exps)
        (let* ((o (comp x env))
               (ostr (with-output-to-string (out)
                       (bard::print-value o out))))
          (format *standard-output* "~% ~A => ~A" x ostr)))
      (terpri *standard-output*)
      (finish-output *standard-output*))))