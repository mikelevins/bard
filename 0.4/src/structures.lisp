;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          structures.lisp
;;;; Project:       Bard
;;;; Purpose:       representation of Bard structures
;;;; Author:        mikel evins
;;;; Copyright:     2013 mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard)

;;; ---------------------------------------------------------------------
;;; <structure>
;;; ---------------------------------------------------------------------
;;; the objects that represent Bard structures
;;; structures are the specifications for constructing families of values
;;; every Bard value is an instance of a structure

(defclass <structure> ()
  ((name :accessor structure-name :initform nil :initarg :name)))

(defclass <primitive-structure> (<structure>)
  ((lisp-type :accessor lisp-type :initform nil :initarg :lisp-type)))

(defmethod print-object ((ps <primitive-structure>)(s stream))
  (format s "#<structure>{name: ~A}" (structure-name ps)))

(defmethod primitive-structure ((name string)(lisp-type symbol))
  (make-instance '<primitive-structure> :name name :lisp-type lisp-type))

;;; ---------------------------------------------------------------------
;;; built-in structures
;;; ---------------------------------------------------------------------

(defparameter *alist-structure* (primitive-structure "<alist>" '<alist>))
(defparameter *bignum-structure* (primitive-structure "<bignum>" 'cl:bignum))
(defparameter *boolean-structure* (primitive-structure "<boolean>" '<boolean>))
(defparameter *character-structure* (primitive-structure "<character>" 'cl:character))
(defparameter *cons-structure* (primitive-structure "<cons>" 'cl:cons))
(defparameter *eof-structure* (primitive-structure "<eof>" '<eof>))
(defparameter *fixnum-structure* (primitive-structure "<fixnum>" 'cl:fixnum))
(defparameter *file-stream-structure* (primitive-structure "<file-stream>" '<file-stream>))
(defparameter *flonum-structure* (primitive-structure "<flonum>" 'cl:float))
(defparameter *function-structure* (primitive-structure "<function>" '<fn>))
(defparameter *keyword-structure* (primitive-structure "<keyword>" 'cl:keyword))
(defparameter *method-structure* (primitive-structure "<method>" '<mfn>))
(defparameter *null-structure* (primitive-structure "<null>" 'cl:null))
(defparameter *ratnum-structure* (primitive-structure "<ratnum>" 'cl:ratio))
(defparameter *stream-structure* (primitive-structure "<stream>" '<stream>))
(defparameter *string-structure* (primitive-structure "<string>" 'cl:string))
(defparameter *symbol-structure* (primitive-structure "<symbol>" 'cl:symbol))
(defparameter *undefined-structure* (primitive-structure "<undefined>" '<undefined>))
(defparameter *url-structure* (primitive-structure "<url>" 'puri:uri))

(defmethod get-structure ((x cl:bignum))
  (declare (ignore x))
  *bignum-structure*)

(defmethod get-structure ((x cl:character))
  (declare (ignore x))
  *character-structure*)

(defmethod get-structure ((x cl:cons))
  (declare (ignore x))
  *cons-structure*)

(defmethod get-structure ((x cl:fixnum))
  (declare (ignore x))
  *fixnum-structure*)

(defmethod get-structure ((x cl:float))
  (declare (ignore x))
  *flonum-structure*)

(defmethod get-structure ((x cl:keyword))
  (declare (ignore x))
  *keyword-structure*)

(defmethod get-structure ((x cl:ratio))
  (declare (ignore x))
  *ratnum-structure*)

(defmethod get-structure ((x cl:string))
  (declare (ignore x))
  *string-structure*)

(defmethod get-structure ((x cl:symbol))
  (declare (ignore x))
  *symbol-structure*)

(defmethod get-structure ((x puri:uri))
  (declare (ignore x))
  *url-structure*)
