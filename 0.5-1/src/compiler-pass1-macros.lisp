;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          compiler-pass1-macros.lisp
;;;; Project:       Bard
;;;; Purpose:       expanding macros
;;;; Author:        mikel evins
;;;; Copyright:     2013 mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard)

;;; ---------------------------------------------------------------------
;;; the macro registry
;;; ---------------------------------------------------------------------

(defun macro-form? (exp)
  (gethash (car exp) $macro-forms nil))

(defparameter $macro-forms (make-hash-table))

(defun def-bard-macro (name expander-fn)
  (setf (gethash name $macro-forms) expander-fn))

;;; ---------------------------------------------------------------------
;;; expanding
;;; ---------------------------------------------------------------------

(defun expand-bard-macro (exp)
  (funcall (gethash (car exp) $macro-forms nil)
           exp))

;;; ---------------------------------------------------------------------
;;; the macros
;;; ---------------------------------------------------------------------

(def-bard-macro 'bard-symbols::|set!|
  (lambda (exp)
    (let* ((place-form (second exp))
           (val-form (third exp)))
      (if (listp place-form)
          `((bard-symbols::|setter| ,(first place-form)) ,(second place-form) ,val-form)
          `((bard-symbols::|setter| ,place-form) ,val-form)))))

(def-bard-macro 'bard-symbols::|with-exit|
  (lambda (exp)
    (warn "with-exit is not yet implemented")))

;;; TODO: this is the right idea, but I should use ensure so that the
;;;       file is guaranteed to be closed, and I should gensym the
;;;       local variable names to avoid variable aliasing
(def-bard-macro 'bard-symbols::|with-open|
  (lambda (exp)
    (let* ((binding-form (second exp))
           (varname (first binding-form))
           (path (second binding-form))
           (open-args (drop 2 binding-form))
           (body-forms (drop 2 exp)))
      `(bard-symbols::|let| ((,varname (bard-symbols::|open| ,path ,@open-args))
                             (_result (bard-symbols::|begin| ,@body-forms)))
                      (bard-symbols::|close| ,varname)
                      _result))))
