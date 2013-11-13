;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          functions.lisp
;;;; Project:       Bard
;;;; Purpose:       implementation of polymorphic functions
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard)

;;; ---------------------------------------------------------------------
;;; functions
;;; ---------------------------------------------------------------------

(defparameter *no-applicable-method*
  (make-instance '<method> 
                 :code '((ERROR "No applicable method"))
                 :name 'bard-symbols::|no-applicable-method|
                 :args '(bard-symbols::|&| args)))

(defclass <function> ()
  ((input-types :accessor function-input-types :initform nil :initarg :input-types)
   (output-types :accessor function-output-types :initform nil :initarg :output-types)
   (method-table :accessor function-method-table :initform (make-method-table) :initarg :method-table)))

(defmethod print-object ((function <function>)(s stream))
  (format s (value->literal-string function)))

(defun make-function (inputs outputs)
  (make-instance '<function> :input-types inputs :output-types outputs))

(defmethod function? (x)
  (declare (ignore x))
  nil)

(defmethod function? ((x <function>))
  (declare (ignore x))
  t)

(defmethod get-structure ((x <function>))
  (declare (ignore x))
  *function-structure*)

(defmethod applicable-method-entries ((function <function>) (args list))
  (remove-if-not (lambda (entry)(matching-types? args (car entry)))
                 (method-table-entries (function-method-table function))))

(defmethod most-specific-method ((function <function>) (args list))
  (let ((method-entries (applicable-method-entries function args)))
    (if (null method-entries)
        *no-applicable-method*
        (let ((ordered-entries (sort method-entries 
                                     (lambda (entry1 entry2)
                                       (more-specific? (car entry1)(car entry2))))))
          (cdr (first ordered-entries))))))

(defmethod assert-method! ((function <function>)(types cons)(method <method>))
  (assert-method! (function-method-table function) types method))

