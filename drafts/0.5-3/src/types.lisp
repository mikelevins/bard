;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          types.lisp
;;;; Project:       Bard
;;;; Purpose:       representation of concrete types
;;;; Author:        mikel evins
;;;; Copyright:     2014 mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard)


;;; ---------------------------------------------------------------------
;;; base types
;;; ---------------------------------------------------------------------

;;; base-type is a subclass of procedure because types are applicable;
;;; applying a type constructs an instance of the type
;;;
;;; TODO: types are currently represented as methods; once
;;; (polymorphic) functions are in, rewrite types to be instances
;;; of functions so that users can specialize them to construct
;;; instances in different ways for different arguments
(defclass base-type (procedure)
  ((name :reader type-name :initarg :name)))

(defmethod print-object ((bt base-type)(out stream))
  (print-unreadable-object (bt out :type t :identity t)
    (format out "~a" (or (name bt)
                         "(anonymous)"))))

(defclass bits-type (base-type)
  ((minimum :reader minimum :initform 0 :initarg :minimum)
   (maximum :reader maximum :initform (undefined) :initarg :maximum)))

(defclass procedure-type (base-type)())

(defclass record-type (base-type)
  ((slot-descriptions :reader slot-descriptions :initform (nothing) :initarg :slot-descriptions)))

(defclass list-type (base-type)
  ((element-type :reader element-type :initform (undefined) :initarg :element-type)
   (minimum-count :reader minimum-count :initform 0 :initarg :minimum-count)
   (maximum-count :reader maximum-count :initform (undefined) :initarg :maximum-count)))

(defclass values-type (base-type)())


(defparameter +bits+ (make-instance 'base-type :name 'bits :args '(n) :env nil
                                    :code (seq
                                           (gen 'ARGS 1)
                                           (gen 'LVAR 0 0)
                                           (gen 'MKBITS)
                                           (gen 'RETURN))))

(defparameter +procedure+ (make-instance 'base-type :name 'procedure :args 'args :env nil
                                         :code (seq
                                                (gen 'ARGS. 0)
                                                (gen 'LVAR 0 0)
                                                (gen 'MKPROC)
                                                (gen 'RETURN))))

(defparameter +list+ (make-instance 'base-type :name 'list :args 'args :env nil
                                    :code (seq
                                           (gen 'ARGS. 0)
                                           (gen 'LVAR 0 0)
                                           (gen 'MKLIST)
                                           (gen 'RETURN))))

(defparameter +record+ (make-instance 'base-type :name 'record :args 'args :env nil
                                      :code (seq
                                             (gen 'ARGS. 0)
                                             (gen 'LVAR 0 0)
                                             (gen 'MKRECORD)
                                             (gen 'RETURN))))

(defparameter +values+ (make-instance 'base-type :name 'values  :args 'args :env nil
                                      :code (seq
                                             (gen 'ARGS. 0)
                                             (gen 'LVAR 0 0)
                                             (gen 'MKVALUES)
                                             (gen 'RETURN))))

;;; ---------------------------------------------------------------------
;;; bits instances
;;; ---------------------------------------------------------------------
;;; the default bits instance is just a Lisp integer

;;; ---------------------------------------------------------------------
;;; procedure instances
;;; ---------------------------------------------------------------------
;;; the default procedure instance is an instance of the Bard class procedure

;;; ---------------------------------------------------------------------
;;; list instances
;;; ---------------------------------------------------------------------
;;; the default list instance is just a Lisp cons-based list

;;; ---------------------------------------------------------------------
;;; record instances
;;; ---------------------------------------------------------------------

(defclass record-instance ()
  ((record-type :reader record-type :initform +record+ :initarg :record-type)
   (slots :accessor slots :initform (fset:map) :initarg :slots)))

(defun make-record-instance (record-type slot-specs)
  (let ((record-type (or record-type +record+))
        (slots (loop for tail on slot-specs by #'cddr
                  collect (cons (car tail)(cadr tail)))))
    (make-instance 'record-instance :record-type record-type 
                   :slots (fset:convert 'fset:map slots))))

;;; ---------------------------------------------------------------------
;;; values instances
;;; ---------------------------------------------------------------------
;;; objects that represent multiple return values

(defclass values-instance ()
  ((vals :accessor vals :initform nil :initarg :vals)))

(defun make-values-instance (vals)
  (make-instance 'values-instance :vals vals))

;;; =====================================================================

;;; ---------------------------------------------------------------------
;;; primitive types
;;; ---------------------------------------------------------------------
;;; primitive types are the type objects that identify native Lisp
;;; representations of values. For example, the underlying
;;; representations of character objects are of Common Lisp type
;;; CHARACTER. The Bard primitive type character is a wrapper for the
;;; native Common Lisp type that provides type-like behavior for it in
;;; the Bard environment. As an example, unlike the Common Lisp
;;; CHARACTER type, the Bard character type is a data constructor that
;;; can be applied to some arguments to construct an instance of
;;; itself.

