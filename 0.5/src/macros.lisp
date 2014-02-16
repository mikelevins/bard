;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          macros.lisp
;;;; Project:       Bard
;;;; Purpose:       definition and implementation of bard macros
;;;; Author:        mikel evins
;;;; Copyright:     2014 mikel evins
;;;;                Incrementally derived from Peter Norvig's Scheme compiler
;;;;                Code from Paradigms of Artificial Intelligence Programming
;;;;                Copyright (c) 1991 Peter Norvig
;;;;
;;;; ***********************************************************************

(in-package :bard)


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

(defun name! (method name)
  "Set the name field of method, if it is an un-named method."
  (when (and (method-p method) (null (method-name method)))
    (setf (method-name method) name))
  name)

(set-global-var! 'name! #'name!)

(def-bard-macro define (name &rest body)
  (if (atom name)
      `(name! (set! ,name . ,body) ',name)
      (bard-macro-expand
       `(define ,(first name) 
            (lambda ,(rest name) . ,body)))))

