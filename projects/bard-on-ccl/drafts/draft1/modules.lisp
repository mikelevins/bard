;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          modules.lisp
;;;; Project:       Bard - a modern Lisp
;;;; Purpose:       modules
;;;; Author:        mikel evins
;;;; Copyright:     2010 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard)

;;; ============================================================
;;; BARD modules
;;; ============================================================

(defparameter *module-table* nil)
(defparameter *bard-module* nil)
(defparameter *module* nil)

(defclass module ()
  ((interned-symbols :reader interned-symbols :initform (make-hash-table :test #'equal))
   (imported-symbols  :reader imported-symbols :initform (make-hash-table :test #'equal))
   (exported-symbols  :reader exported-symbols :initform (make-hash-table :test #'equal))
   (modules-used :reader modules-used :initform nil)
   (modules-using :reader modules-using :initform nil)))

(defmethod print-object ((m module)(s stream))
  (format s "#<bard module ~A>" (ccl::sxhash m)))

(defun find-module (modpath)
  (gethash modpath *module-table* nil))

(defun find-symbol (mod sname)
  (gethash sname (interned-symbols mod) nil))

(defun add-module (modpath)
  (unless (find-module modpath)
    (setf (gethash modpath *module-table*)
          (make-instance 'module))))

(defmethod intern ((s string)(m module))
  (setf (gethash s (interned-symbols m))
        (symbol s)))

(defun init-modules ()
  (setf *module-table* (make-hash-table :test #'equal))
  ;; module bard
  (add-module "bard")
  (setf *bard-module* (find-module "bard"))
  ;; var bard.*module*
  (setf *module* *bard-module*)
  (intern "*module*" (find-module "bard"))
  (alter-toplevel-environment! 
   (add-binding (bard-toplevel-environment)
                (bard::module-qualified-symbol (bard:symbol "bard.*module*"))
                *module*)))

(defun module-qualified-symbol? (sym)
  (find #\. (cl:symbol-name (name sym))))

(defun unqualified-symbol? (sym)
  (not (module-qualified-symbol? sym)))

(defun parse-module-qualified-symbol (sym)
  (let* ((sname (cl:symbol-name (name sym)))
         (split-pos (position #\. sname :from-end t)))
    (values (subseq sname 0 split-pos)
            (subseq sname (1+ split-pos)))))

(defun module-qualified-symbol (sym)
  (multiple-value-bind (modpath sname) (parse-module-qualified-symbol sym)
    (let ((mod (find-module modpath)))
      (find-symbol mod sname))))

(defun get-current-module (env)
  (lookup-variable (module-qualified-symbol (symbol "bard.*module*")) env))

(defun find-symbol-in-current-module (sym env)
  (let ((mod (get-current-module env)))
    (find-symbol mod (name sym))))

