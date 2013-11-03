;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          instructions.lisp
;;;; Project:       Bard
;;;; Purpose:       tools for handling instructions
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard)

;;; ---------------------------------------------------------------------
;;; instruction utilities
;;; ---------------------------------------------------------------------

(defun arg-count (form min &optional (max min))
  "Report an error if form has wrong number of args."
  (let ((n-args (length (rest form))))
    (assert (<= min n-args max) (form)
      "Wrong number of arguments for ~a in ~a: ~d supplied, ~d~@[ to ~d~] expected"
      (first form) form n-args min (if (/= min max) max))))

(defun label? (x) "Is x a label?" (atom x))

(defun opcode (instr) (if (label? instr) :label (first instr)))
(defun args (instr) (if (listp instr) (rest instr)))

(defun arg1 (instr) (if (listp instr) (second instr)))
(defun arg2 (instr) (if (listp instr) (third instr)))
(defun arg3 (instr) (if (listp instr) (fourth instr)))

(defun is (instr op)
  "True if instr's opcode is OP, or one of OP when OP is a list."
  (if (listp op) 
      (member (opcode instr) op)
      (eq (opcode instr) op)))
