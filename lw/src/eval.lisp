;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          eval.lisp
;;;; Project:       Bard
;;;; Purpose:       the Bard evaluator
;;;; Author:        mikel evins
;;;; Copyright:     2011 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard)

(defmethod eval (expr (bard bard-runtime))
  (error "Unrecognized expression for evaluation: ~S" expr))

;;; ---------------------------------------------------------------------
;;; self-evaluating
;;; ---------------------------------------------------------------------

(defmethod eval ((expr undefined) (bard bard-runtime)) expr)
(defmethod eval ((expr nothing) (bard bard-runtime)) expr)
(defmethod eval ((expr true) (bard bard-runtime)) expr)
(defmethod eval ((expr false) (bard bard-runtime)) expr)
(defmethod eval ((expr integer) (bard bard-runtime)) expr)
(defmethod eval ((expr float) (bard bard-runtime)) expr)
(defmethod eval ((expr character) (bard bard-runtime)) expr)
(defmethod eval ((expr sequence) (bard bard-runtime)) expr)
(defmethod eval ((expr map) (bard bard-runtime)) expr)

;;; ---------------------------------------------------------------------
;;; variables
;;; ---------------------------------------------------------------------

(defmethod eval ((expr name) (bard bard-runtime)) 
  (error "Variable evaluation is not yet implemented"))

;;; ---------------------------------------------------------------------
;;; applications
;;; ---------------------------------------------------------------------

(defmethod eval ((expr application) (bard bard-runtime)) 
  (error "Function application is not yet implemented"))
