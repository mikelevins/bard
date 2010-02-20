;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: asdf -*-
;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          printer.lisp
;;;; Project:       Bard - a modern Lisp
;;;; Purpose:       the printer
;;;; Author:        mikel evins
;;;; Copyright:     2010 by mikel evins
;;;;
;;;; ***********************************************************************

;;; ============================================================
;;; BARD printer
;;; ============================================================

(in-package :bard)

(defmethod print-value (val (out stream))
  (print-unreadable-object (val out)
    (format out "~A " "Lisp value: ")
    (print-object val out)
    (values)))

(defmethod print-value ((val module) (out stream))
  (print-object val out)
  (values))

(defmethod print-value ((val void) (out stream))
  (princ "void" out)
  (values))

(defmethod print-value ((val number) (out stream))
  (princ (value val) out)
  (values))

(defmethod print-value ((val cl:character) (out stream))
  (princ #\\ out)
  (princ val out)
  (values))

(defmethod print-value ((val keyword) (out stream))
  (princ (name val) out)
  (princ #\: out)
  (values))

(defmethod print-value ((val symbol) (out stream))
  (princ (name val) out)
  (values))

(defmethod print-value ((val true) (out stream))
  (princ "true" out)
  (values))

(defmethod print-value ((val false) (out stream))
  (princ "false" out)
  (values))

(defmethod print-value ((val cl:cons) (out stream))
  (princ "(" out)
  (print-value (left val) out)
  (princ " , " out)
  (print-value (right val) out)
  (princ ")" out)
  (values))

(defun %print-general-sequence (val out)
  (princ "(" out)
  (let ((s val))
    (block printing-elts
      (loop
         (when (fset:empty? s) (return-from printing-elts 'done))
         (let ((o (fset:first s))
               (more (fset:less-first s)))
           (print-value o out)
           (unless (fset:empty? more)
             (princ " " out))
           (setf s more)))))
  (princ ")" out)
  (values))

(defun %print-text-sequence (val out)
  (princ "\"" out)
  (fset:do-seq (c val)
    (princ c out))
  (princ "\"" out)
  (values))

(defmethod print-value ((val fset:seq) (out stream))
  (if (text? val)
      (%print-text-sequence val out)
      (%print-general-sequence val out)))


(defmethod print-value ((val fset:map) (out stream))
  (if (fset:empty? val)
      (princ "{}" out)
      (let ((curr-k nil)(curr-v nil)
            (next-k nil)(next-v nil))
        (princ "{" out)
        (fset:do-map (k v val)
          (setf curr-k next-k curr-v next-v)
          (setf next-k k next-v v)
          (when curr-k
            (print-value curr-k out)
            (princ " " out)
            (print-value curr-v out)
            (princ ", " out)))
        (when next-k
          (print-value next-k out)
          (princ " " out)
          (print-value next-v out))
        (princ "}" out))))
