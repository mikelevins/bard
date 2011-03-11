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

(defmethod eval (expr (env environment)(bard bard-runtime))
  (error "Unrecognized expression for evaluation: ~S" expr))

;;; ---------------------------------------------------------------------
;;; self-evaluating
;;; ---------------------------------------------------------------------

(defmethod eval ((expr undefined) (env environment) (bard bard-runtime)) expr)
(defmethod eval ((expr nothing) (env environment) (bard bard-runtime)) expr)
(defmethod eval ((expr true) (env environment) (bard bard-runtime)) expr)
(defmethod eval ((expr false) (env environment) (bard bard-runtime)) expr)
(defmethod eval ((expr integer) (env environment) (bard bard-runtime)) expr)
(defmethod eval ((expr float) (env environment) (bard bard-runtime)) expr)
(defmethod eval ((expr character) (env environment) (bard bard-runtime)) expr)
(defmethod eval ((expr sequence) (env environment) (bard bard-runtime)) expr)
(defmethod eval ((expr map) (env environment) (bard bard-runtime)) expr)

;;; ---------------------------------------------------------------------
;;; variables
;;; ---------------------------------------------------------------------

(defmethod eval ((expr name) (env environment) (bard bard-runtime)) 
  (let* ((mname (module-name expr))
         (module (find-module bard mname))
         (env-val (getvar env expr)))
    (if (defined? env-val)
        env-val
        (if module
            (let ((mval (getvar module expr)))
              (if (defined? mval)
                  mval
                  (error "Undefined variable:" expr)))
            (error "Undefined variable:" expr)))))

;;; ---------------------------------------------------------------------
;;; applications
;;; ---------------------------------------------------------------------

(defmethod eval ((expr application) (env environment) (bard bard-runtime)) 
  (error "Function application is not yet implemented"))
