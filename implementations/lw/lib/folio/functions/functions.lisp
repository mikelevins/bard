;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          functions.lisp
;;;; Project:       folio - the Bard runtime
;;;; Purpose:       combinators
;;;; Author:        mikel evins
;;;; Copyright:     2010 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(defpackage "FOLIO.FUNCTIONS"
  (:use :cl :fn)
  (:nicknames "FUN")
  (:export "COMPOSE" "CONJOIN" "DISJOIN" "FLIP" "PARTIAL"
           "ROTATE-LEFT" "ROTATE-RIGHT"))

(in-package :FUN)

;;; =====================================================================
;;; FUNCTIONS API
;;; =====================================================================

;;; ---------------------------------------------------------------------
;;; compose
;;; ---------------------------------------------------------------------

(defun compose (f &rest fs)
  (if (null fs)
      f
      (if (null (cdr fs))
          (fn (x) ($ f ($ (car fs) x)))
          (fn (x) ($ f ($ (apply 'compose fs) x))))))

;;; ---------------------------------------------------------------------
;;; conjoin
;;; ---------------------------------------------------------------------

(defun conjoin (&rest preds)
  (fn (&rest args)
    (let ((val nil))
      (some (fn (p) 
              (setf val (apply p args))
              (not val))
            preds)
      val)))

;;; ---------------------------------------------------------------------
;;; disjoin
;;; ---------------------------------------------------------------------

(defun disjoin (&rest preds)
  (fn (&rest args)
    (let ((val nil))
      (some (fn (p) 
              (setf val (apply p args))
              val)
            preds)
      val)))

;;; ---------------------------------------------------------------------
;;; flip
;;; ---------------------------------------------------------------------

(defun flip (f) (fn (x y) ($ f y x)))

;;; ---------------------------------------------------------------------
;;; partial
;;; ---------------------------------------------------------------------

(defun partial (f &rest args)
  (lambda (&rest more-args)
    (apply f `(,@args ,@more-args))))

;;; ---------------------------------------------------------------------
;;; rotate-left
;;; ---------------------------------------------------------------------

(defun rotate-left (f)
  (fn (&rest args)
    (apply f `(,@(last args) ,@(subseq args 0 (1- (length args)))))))

;;; ---------------------------------------------------------------------
;;; rotate-right
;;; ---------------------------------------------------------------------

(defun rotate-right (f)
  (fn (&rest args)
    (apply f `(,@(cdr args) ,(car args)))))
