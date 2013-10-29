;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          utils.lisp
;;;; Project:       Bard
;;;; Purpose:       general-purpose utilities
;;;; Author:        mikel evins, after Norvig
;;;; Copyright:     1991 by Peter Norvig, 2013 mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard)

;;; ---------------------------------------------------------------------
;;; errors, warnings, etc.
;;; ---------------------------------------------------------------------

(defun not-yet-implemented (name &optional (msg "Not yet implemented"))
  (error "~a: ~a" msg name))

;;; ---------------------------------------------------------------------
;;; instruction-list manipulations 
;;; ---------------------------------------------------------------------

(defun length=1? (x) 
  (and (consp x) (null (cdr x))))

(defun mappend (fn list)
  (apply #'append (mapcar fn list)))

(defun maybe-add (op exps &optional if-nil)
  (cond ((null exps) if-nil)
        ((length=1? exps) (first exps))
        (t (cons op exps))))

(defun rest2 (x)(rest (rest x)))
(defun rest3 (x)(rest (rest (rest x))))

(defun starts-with? (list x)
  (and (consp list)
       (eql (first list) x)))

(defun make-true-list (dotted-list)
  "Convert a possibly dotted list into a true, non-dotted list."
  (cond ((null dotted-list) nil)
        ((atom dotted-list) (list dotted-list))
        (t (cons (first dotted-list)
                 (make-true-list (rest dotted-list))))))

(defun times (n x)
  (if (< n 1)
      nil
      (cons x (times (- n 1) x))))
