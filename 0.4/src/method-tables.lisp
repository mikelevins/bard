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

(defun assert-method! ((table <method-table>)(types cons)(method <method>))
  (let ((entry (assoc types (entries table) :test 'equal)))
    (if entry
        (setf (cdr entry) method)
        (setf (entries table)
              (cons (cons (copy-tree types) method)
                    (entries table))))
    table))

(defun find-methods ((table <method-table>)(vals cons))
  (mapcar (lambda (entry)
            (if (matching-types? vals (car entry))
                (cdr entry)
                nil)) 
          (entries table)))


