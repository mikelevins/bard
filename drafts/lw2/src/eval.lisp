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

(defun special-form? (op) 
  (warn "special-form? is not yet implemented")
  nil)

(defun apply-special-form (op args env bard)
  )

(defun macro? (op bard)
  (warn "macro? is not yet implemented")
  nil)

(defun apply-macro (op args env bard)
  )

(defmethod apply ((op primitive) args env bard)
  (when (arg-count op)
    (unless (= (length args)(arg-count op))
      (error "Primitive ~A requires ~A arguments, but received ~A"
             (debug-name op)(arg-count op)(length args))))
  (funcall (code op) args))

(defmethod apply ((fn function) args env bard)
  (warn "apply function is not yet implemented"))

(defmethod apply ((m method) args env bard)
  (warn "apply method is not yet implemented"))

(defmethod apply ((m map) args env bard)
  (warn "apply map is not yet implemented"))

(defmethod eval ((expr application) (env environment) (bard bard-runtime)) 
  (if (zerop (length expr))
      (empty-sequence)
      (let ((op (first expr))
            (args (rest expr)))
        (cond
          ((special-form? op)(apply-special-form op args env bard))
          ((macro? op bard)(apply-macro op args env bard))
          (t (let ((op (eval op env bard))
                   (args (map-over (lambda (a)(eval a env bard))
                                   args)))
               (apply op args env bard)))))))

