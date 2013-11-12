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
  ())

(defun make-method-table ()
  (make-instance '<method-table>))





