;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          method-tables.lisp
;;;; Project:       Bard
;;;; Purpose:       implementation of method tables
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard)

;;; ---------------------------------------------------------------------
;;; method tables
;;; ---------------------------------------------------------------------
;;; method tables store methods in a way that functions can easily
;;; find matching methods for argument sets
;;;
;;; types of dispatch:
;;; (define method (foo x y))
;;;   no dispatch; choose the default, unspecialized method.
;;;   equivalent to:
;;; (define method (foo x y)
;;;   where {x: Anything y: Anything})
;;;
;;; (define method (foo x y)
;;;   where {x: <fixnum> y: <string>} 
;;;   ...)
;;;   representation dispatch: choose the method for an argument
;;;   list whose representations are [<fixnum> <string>]
;;;
;;; (define method (foo x y)
;;;   where {x: (exactly 5) y: (exactly "Hello")} 
;;;   ...)
;;;   representation dispatch: choose the method for an argument
;;;   list whose representations are [<fixnum> <string>]


(defclass <method-table> ()
  ((entries :accessor method-table-entries :initform nil :initarg :entries)))

(defun make-method-table ()
  (make-instance '<method-table>))

(defmethod assert-method! ((table <method-table>)(types cons)(method <method>))
  (let ((entry (assoc types (method-table-entries table) :test 'equal)))
    (if entry
        (setf (cdr entry) method)
        (setf (method-table-entries table)
              (cons (cons (copy-tree types) method)
                    (method-table-entries table))))
    table))

(defun more-specific? (types1 types2)
  (if (null types1)
      nil
      (if (null types2)
          t
          (let ((type1 (car types1))
                (type2 (car types2)))
            (if (equal type1 type2)
                (more-specific? (cdr types1)(cdr types2))
                (if (subtype? type1 type2)
                    t
                    nil))))))
