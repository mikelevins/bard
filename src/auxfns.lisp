;;;; -*- Mode: Lisp; Syntax: Common-Lisp -*-
;;;; ---------------------------------------------------------------------
;;;; bardvm
;;;; A VM implementation based on Norvig's Scheme compiler from PAIP
;;;; ---------------------------------------------------------------------

;;; Code from Paradigms of AI Programming
;;; Copyright (c) 1991 Peter Norvig

;;; File auxfns.lisp: Auxiliary functions used by all other programs
;;; Load this file before running any other programs.

;;; integrated into bardvm by mikel evins, mikel@evins.net, July 2020

(in-package :bardvm)

;;;; ---------------------------------------------------------------------
;;;; Implementation-Specific Details
;;;; ---------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; Make it ok to place a function definition on a built-in LISP symbol.
  #+(or Allegro EXCL)
  (dolist (pkg '(excl common-lisp common-lisp-user))
    (setf (excl:package-definition-lock (find-package pkg)) nil))

  ;; Don't warn if a function is defined in multiple files --
  ;; this happens often since we refine several programs.
  #+Lispworks
  (setq *PACKAGES-FOR-WARN-ON-REDEFINITION* nil)

  #+LCL
   (compiler-options :warnings nil)
  #+sbcl
  (progn
    (sb-ext:unlock-package '#:common-lisp)
    (sb-ext:unlock-package '#:common-lisp-user)))


;;;; ---------------------------------------------------------------------
;;;; Auxiliary Functions
;;;; ---------------------------------------------------------------------

(defun rest2 (x)
  "The rest of a list after the first TWO elements."
  (rest (rest x)))

(defun starts-with (list x)
  "Is x a list whose first element is x?"
  (and (consp list) (eql (first list) x)))

(defun maybe-add (op exps &optional if-nil)
  "For example, (maybe-add 'and exps t) returns
  t if exps is nil, exps if there is only one,
  and (and exp1 exp2...) if there are several exps."
  (cond ((null exps) if-nil)
        ((length=1 exps) (first exps))
        (t (cons op exps))))

;;; ==============================

(defun last1 (list)
  "Return the last element (not last cons cell) of list"
  (first (last list)))

;;;; ---------------------------------------------------------------------
;;;; Other:
;;;; ---------------------------------------------------------------------

(defun length=1 (x)
  "Is x a list of length 1?"
  (and (consp x) (null (cdr x))))

(defun rest3 (list)
  "The rest of a list after the first THREE elements."
  (cdddr list))

