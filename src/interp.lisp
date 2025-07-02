;;;; ***********************************************************************
;;;;
;;;; Name:          interp.lisp
;;;; Project:       the bard programming lnaguage
;;;; Purpose:       bard interpreter
;;;; Author:        mikel evins
;;;; Copyright:     2025 by mikel evins
;;;; Code from Paradigms of AI Programming
;;;; Copyright (c) 1991 Peter Norvig
;;;  File: interp1.lisp: simple Bard interpreter, including macros.
;;;;
;;;; ***********************************************************************

(in-package :bard)

;;; ==============================

(defun bard-macro (symbol)
  (and (symbolp symbol) (get symbol 'bard-macro)))

(defmacro def-bard-macro (name parmlist &body body)
  "Define a Bard macro."
  `(setf (get ',name 'bard-macro)
         #'(lambda ,parmlist .,body)))

(defun bard-macro-expand (x)
  "Macro-expand this Bard expression."
  (if (and (listp x) (bard-macro (first x)))
      (bard-macro-expand
        (apply (bard-macro (first x)) (rest x)))
      x))

;;; ==============================

(defun set-var! (var val env)
  "Set a variable to a value, in the given or global environment."
  (if (assoc var env)
      (setf (second (assoc var env)) val)
      (set-global-var! var val))
  val)

(defun get-var (var env)
  "Get the value of a variable, from the given or global environment."
    (if (assoc var env)
        (second (assoc var env))
        (get-global-var var)))

(defun set-global-var! (var val)
  (setf (get var 'global-val) val))

(defun get-global-var (var)
  (let* ((default "unbound")
         (val (get var 'global-val default)))
    (if (eq val default)
        (error "Unbound bard variable: ~a" var)
        val)))

(defun extend-env (vars vals env)
  "Add some variables and values to an environment."
  (nconc (mapcar #'list vars vals) env))

;;; ==============================

(def-bard-macro let (bindings &rest body)
  `((lambda ,(mapcar #'first bindings) . ,body)
    .,(mapcar #'second bindings)))

(def-bard-macro let* (bindings &rest body)
  (if (null bindings)
      `(begin .,body)
      `(let (,(first bindings))
         (let* ,(rest bindings) . ,body))))

(def-bard-macro and (&rest args)
  (cond ((null args) 'T)
        ((length=1 args) (first args))
        (t `(if ,(first args)
                (and . ,(rest args))))))

(def-bard-macro or (&rest args)
  (cond ((null args) 'nil)
        ((length=1 args) (first args))
        (t (let ((var (gensym)))
             `(let ((,var ,(first args)))
                (if ,var ,var (or . ,(rest args))))))))

(def-bard-macro cond (&rest clauses)
  (cond ((null clauses) nil)
        ((length=1 (first clauses))
         `(or ,(first clauses) (cond .,(rest clauses))))
        ((starts-with (first clauses) 'else)
         `(begin .,(rest (first clauses))))
        (t `(if ,(first (first clauses))
                (begin .,(rest (first clauses)))
                (cond .,(rest clauses))))))

(def-bard-macro case (key &rest clauses)
  (let ((key-val (gensym "KEY")))
    `(let ((,key-val ,key))
       (cond ,@(mapcar
                #'(lambda (clause)
                    (if (starts-with clause 'else)
                        clause
                        `((member ,key-val ',(first clause))
                          .,(rest clause))))
                clauses)))))

(def-bard-macro define (name &rest body)
  (if (atom name)
      `(begin (set! ,name . ,body) ',name)
      `(define ,(first name)
         (lambda ,(rest name) . ,body))))

(def-bard-macro delay (computation)
  `(lambda () ,computation))

(def-bard-macro letrec (bindings &rest body)
  `(let ,(mapcar #'(lambda (v) (list (first v) nil)) bindings)
     ,@(mapcar #'(lambda (v) `(set! .,v)) bindings)
     .,body))
