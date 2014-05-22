;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          prims.lisp
;;;; Project:       Bard
;;;; Purpose:       VM primitives
;;;; Author:        mikel evins
;;;; Copyright:     2014 mikel evins
;;;;                Incrementally derived from Peter Norvig's Scheme compiler
;;;;                Code from Paradigms of Artificial Intelligence Programming
;;;;                Copyright (c) 1991 Peter Norvig
;;;;
;;;; ***********************************************************************

(in-package :bard)

;;; ==============================

(defparameter *primitive-fns*
  '((+ 2 + true) (- 2 - true) (* 2 * true) (/ 2 / true)
    (< 2 <) (> 2 >) (<= 2 <=) (>= 2 >=) (/= 2 /=) (= 2 =)
    (eq? 2 eq) (equal? 2 equal) (eqv? 2 eql)
    (not 1 not) (null? 1 not)
    (car 1 car) (cdr 1 cdr)  (cadr 1 cadr) (cons 2 cons true)
    (read 0 bard-read nil t) (end? 1 end?)
    (write 1 write nil t) (display 1 display nil t)
    (newline 0 newline nil t) (compiler 1 compiler t) 
    (name! 2 name! true t) (random 1 random true nil)
    ;; base-type constructors
    (mkbits 1 mkbits true)
    (mklist 1 mklist true)
    (mkrecord 1 mkrecord true)
    (mkvalues 1 mkvalues true)))

;;; ==============================

(defstruct (prim (:type list)) 
  symbol n-args opcode always side-effects)

(defun primitive-p (f env n-args)
  "F is a primitive if it is in the table, and is not shadowed
  by something in the environment, and has the right number of args."
  (and (not (in-env-p f env))
       (find f *primitive-fns*
             :test #'(lambda (f prim)
                       (and (eq f (prim-symbol prim))
                            (= n-args (prim-n-args prim)))))))

(defun list1 (x) (list x))
(defun list2 (x y) (list x y))
(defun list3 (x y z) (list x y z))
(defun newline () (terpri))
(defun mkbits (n) (if (integerp n) n (error "bits constructor expected an integer but found ~S" n)))
(defun mklist (elts) elts)
(defun mkrecord (slots)(make-record-instance nil slots))
(defun mkvalues (vals)(make-values-instance vals))
