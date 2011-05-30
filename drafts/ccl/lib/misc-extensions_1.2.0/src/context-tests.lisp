;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: Lexical-Contexts -*-

;;; This file is in the public domain.  It is provided with ABSOLUTELY 
;;; NO WARRANTY.

(in-package :lexical-contexts)

(defcontext context-0 (x y #'f &optional (z 3))
  "X and Y are numbers; F is a function from integers to integers such that,
if iterated from any initial argument, it sooner or later returns an odd
result."

  (deflex a (+ x y)
    "The sum of X and Y.")

  (defun foo (q)
    "Does something weird.  Calls BAR."
    (list x y a z (bar (f q))))

  (defun bar (m)
    "Does something else weird.  May call FOO."
    (if (oddp m) (foo m) (list m))))

(defun test-context-0 ()
  (with-context (context-0 13 47 #'1+)
    (let ((result (foo 22)))
      (unless (equal result '(13 47 60 3 (13 47 60 3 (24))))
	(error "test-context-0 failed: ~S" result)))))

(defcontext context-1 (k)
  "A context that imports context-0."

  (import-context (context-0 22 16 (lambda (x) (1+ (* k x))) k))

  (defun baz (n)
    (foo (* x n))))

(defun test-context-1 ()
  (with-context (context-1 17)
    (let ((result (baz 42)))
      (unless (equal result '(22 16 38 17 (22 16 38 17 (267054))))
	(error "test-context-1 failed: ~S" result)))))

(test-context-1)

(defcontext context-2 (x)
  "Just to test macros."

  (defmacro foo (arg) `(list ',arg ,arg))

  (defun bar ()
    (foo x)))

(defun test-context-2 ()
  (let ((result (with-context (context-2 37) (list (bar) (foo x)))))
    (unless (equal result '((x 37) (x 37)))
      (error "test-context-2 failed: ~S" result))))

(test-context-2)

;;; Test context redefinition that omits names.
#|| Doesn't work compiled (at least in SBCL); have to do it manually.
(defcontext context-3 (x)
  (deflex y (+ x 13)))

(defun test-context-3-a ()
  (with-context (context-3 17)
    (list y)))

(defcontext context-3 (x))

(defun test-context-3 ()
  (let ((result (test-context-3-a)))
    (unless (equal result (list context-instance-default-element))
      (error "test-context-3 failed: ~S" result))))
||#
