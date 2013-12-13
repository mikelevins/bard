;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          utils.lisp
;;;; Project:       Bard
;;;; Purpose:       general-purpose utilities
;;;; Author:        mikel evins
;;;; Copyright:     2013 mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard)

;;; ---------------------------------------------------------------------
;;; list utilities
;;; ---------------------------------------------------------------------

(defun alist->plist (alist)
  (reduce 'append
          (loop for pair in alist
             collect (list (car pair)
                           (cdr pair)))))

(defun plist->alist (plist)
  (loop for p on plist by 'cddr 
     collect (cons (first p)
                   (second p))))

(defun take (n list)
  (subseq list 0 n))

(defun drop (n list)
  (nthcdr n list))
