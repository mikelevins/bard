;;;; ***********************************************************************
;;;;
;;;; Name:          auxfns.lisp
;;;; Project:       the bard programming lnaguage
;;;; Purpose:       auxiliary functions
;;;; Author:        mikel evins
;;;; Copyright:     2025 by mikel evins
:;;; Code from Paradigms of AI Programming
:;;; Copyright (c) 1991 Peter Norvig
;;;;
;;;; ***********************************************************************

(in-package :bard)

(eval-when (eval compile load)
  #+sbcl
  (progn
    (sb-ext:unlock-package '#:common-lisp)
    (sb-ext:unlock-package '#:common-lisp-user)))

;;;; Macros (formerly in auxmacs.lisp: that file no longer needed)

(eval-when (load eval compile)
  (defmacro once-only (variables &rest body)
    "Returns the code built by BODY.  If any of VARIABLES
  might have side effects, they are evaluated once and stored
  in temporary variables that are then passed to BODY."
    (assert (every #'symbolp variables))
    (let ((temps nil))
      (dotimes (i (length variables)) (push (gensym) temps))
      `(if (every #'side-effect-free? (list .,variables))
	   (progn .,body)
	   (list 'let
	         ,`(list ,@(mapcar #'(lambda (tmp var)
			               `(list ',tmp ,var))
			           temps variables))
	         (let ,(mapcar #'(lambda (var tmp) `(,var ',tmp))
		               variables temps)
	           .,body)))))

  (defun side-effect-free? (exp)
    "Is exp a constant, variable, or function,
  or of the form (THE type x) where x is side-effect-free?"
    (or (atom exp) (constantp exp)
	(starts-with exp 'function)
	(and (starts-with exp 'the)
	     (side-effect-free? (third exp)))))

  (defmacro funcall-if (fn arg)
    (once-only (fn)
	       `(if ,fn (funcall ,fn ,arg) ,arg)))

  (defmacro read-time-case (first-case &rest other-cases)
    "Do the first case, where normally cases are
  specified with #+ or possibly #- marks."
    (declare (ignore other-cases))
    first-case)

  (defun rest2 (x)
    "The rest of a list after the first TWO elements."
    (rest (rest x)))

  (defun starts-with (list x)
    "Is x a list whose first element is x?"
    (and (consp list) (eql (first list) x)))
  )

;;; ---------------------------------------------------------------------
;;; Auxiliary Functions
;;; ---------------------------------------------------------------------

(defun ->symbol (&rest args)
  "Concatenate symbols or strings to form an interned symbol"
  (intern (format nil "~{~a~}" args)))

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

;;; ==============================

(defun mappend (fn list)
  "Append the results of calling fn on each element of list.
  Like mapcon, but uses append instead of nconc."
  (apply #'append (mapcar fn list)))


;;; ---------------------------------------------------------------------
;;; Other:
;;; ---------------------------------------------------------------------

(defun length=1 (x)
  "Is x a list of length 1?"
  (and (consp x) (null (cdr x))))

(defun rest3 (list)
  "The rest of a list after the first THREE elements."
  (cdddr list))



