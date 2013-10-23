;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          modules.lisp
;;;; Project:       Bard
;;;; Purpose:       bard modules
;;;; Author:        mikel evins
;;;; Copyright:     2013 mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard)

;;; ---------------------------------------------------------------------
;;; module names
;;; ---------------------------------------------------------------------

(defmethod valid-module-name? (x)(declare (ignore x)) nil)

(defmethod valid-module-name? ((x symbol))
  (let ((s (symbol-name x)))
    (and (alpha-char-p (elt s 0))
         (every (lambda (c) 
                  (or (char= c #\.)
                      (alphanumericp c))) 
                s))))

(deftype module-name ()
  `(and symbol
        (satisfies valid-module-name?)))

(defun current-module-name ()
  (intern "bard.user" (find-package :bard-modules)))

;;; ---------------------------------------------------------------------
;;; module variables
;;; ---------------------------------------------------------------------

(defclass <mvar> ()
  (name id mutable? exported? import-from import-name))

(defun make-mvar (name id &key (mutable t) (exported nil) (import-from nil) (import-name nil))
  (make-instance '<mvar> name id :mutable mutable :exported exported :import-from import-from :import-name import-name))

;;; ---------------------------------------------------------------------
;;; module names
;;; ---------------------------------------------------------------------

(defclass <module> ()
  ((variables :accessor variables :initform (make-hash-table))))

(defun make-module ()(make-instance '<module>))



;;; ---------------------------------------------------------------------
;;; module registry
;;; ---------------------------------------------------------------------

(defparameter *module-registry* (make-hash-table))

(defun defmodule (mname)
  (assert (typep mname 'module-name)()
          "Invalid module name ~S" mname)
  (setf (gethash mname *module-registry*)
        (make-module)))
