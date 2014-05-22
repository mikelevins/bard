;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          methods.lisp
;;;; Project:       Bard
;;;; Purpose:       implementation of method functions
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard)

;;; ---------------------------------------------------------------------
;;; method-functions
;;; ---------------------------------------------------------------------

(defclass <method> ()
  ((expression :accessor method-expression :initform nil :initarg :expression)
   (code :accessor method-code :initform nil :initarg :code)
   (env :accessor method-env :initform nil :initarg :env)
   (name :accessor method-name :initform nil :initarg :name)
   (args :accessor method-args :initform nil :initarg :args)
   (restarg :accessor method-restarg :initform nil :initarg :restarg)))

(defmethod make-call-env ((method <method>)(base-env <environment>)(args list))
  (let* ((argnames (method-args method))
         (required-count (length argnames))
         (found-count (length args))
         (method-env (method-env method))
         (args-env (cond
                     ((= found-count required-count) (make-environment argnames args))
                     ((< found-count required-count) (error "Not enough arguments; expected ~a but found ~a"
                                                            required-count found-count))
                     ((> found-count required-count) (let ((restarg (method-restarg method)))
                                                       (if restarg
                                                           (let* ((required-vals (subseq args 0 required-count))
                                                                  (rest-vals (subseq args required-count))
                                                                  (all-vals (append required-vals (list rest-vals)))
                                                                  (all-params (append argnames (list restarg))))
                                                             (make-environment all-params all-vals))
                                                           (error "Too many arguments; expected ~a but found ~a"
                                                                  required-count found-count)))))))
    (merge-environments (merge-environments base-env method-env)
                        args-env)))


(defmethod get-structure ((x <method>))
  (declare (ignore x))
  *method-structure*)

(defmethod method? (x)
  (declare (ignore x))
  nil)

(defmethod method? ((x <method>))
  (declare (ignore x))
  t)
