;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          utils.lisp
;;;; Project:       Bard
;;;; Purpose:       general-purpose utilities
;;;; Author:        mikel evins
;;;; Copyright:     2013 mikel evins
;;;;                Portions copyright 1991 by Peter Norvig
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

(defmethod drop ((n integer) (ls null))
  (cond
    ((< n 0) (error "The count argument to drop must be a non-negative integer"))
    ((= n 0) ls)
    (t (error "Can't drop elements from the empty list"))))

(defmethod drop ((n integer) (ls cons))
  (cond
    ((< n 0) (error "The count argument to drop must be a non-negative integer"))
    ((= n 0) ls)
    (t (drop (- n 1) (cdr ls)))))

(defmethod take ((n (eql 0)) (ls null))
  ls)

(defmethod take ((n integer) (ls null))
  (error "Can't take ~a elements from an empty list" n))

(defmethod take ((n (eql 0)) (ls cons))
  ls)

(defmethod take ((n integer) (ls cons))
  (subseq ls 0 n))

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
