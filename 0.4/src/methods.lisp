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

(defclass <mfn> ()
  ((expression :accessor mfn-expression :initform nil :initarg :expression)
   (code :accessor mfn-code :initform nil :initarg :code)
   (env :accessor mfn-env :initform nil :initarg :env)
   (name :accessor mfn-name :initform nil :initarg :name)
   (args :accessor mfn-args :initform nil :initarg :args)
   (restarg :accessor mfn-restarg :initform nil :initarg :restarg)))

(defmethod make-call-env ((mfn <mfn>)(base-env <environment>)(args list))
  (let* ((argnames (mfn-args mfn))
         (required-count (length argnames))
         (found-count (length args))
         (mfn-env (mfn-env mfn))
         (args-env (cond
                     ((= found-count required-count) (make-environment argnames args))
                     ((< found-count required-count) (error "Not enough arguments; expected ~a but found ~a"
                                                            required-count found-count))
                     ((> found-count required-count) (let ((restarg (mfn-restarg mfn)))
                                                       (if restarg
                                                           (let* ((required-vals (subseq args 0 required-count))
                                                                  (rest-vals (subseq args required-count))
                                                                  (all-vals (append required-vals (list rest-vals)))
                                                                  (all-params (append argnames (list restarg))))
                                                             (make-environment all-params all-vals))
                                                           (error "Too many arguments; expected ~a but found ~a"
                                                                  required-count found-count)))))))
    (merge-environments (merge-environments base-env mfn-env)
                        args-env)))

