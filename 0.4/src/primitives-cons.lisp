;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          primitives-cons.lisp
;;;; Project:       Bard
;;;; Purpose:       primitives for working with cons cells
;;;; Author:        mikel evins
;;;; Copyright:     2013 mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard)

;;; ---------------------------------------------------------------------
;;; cons constructors
;;; ---------------------------------------------------------------------

(defun list0 () nil)
(defun list1 (a) (list a))
(defun list2 (a b) (list a b))
(defun list3 (a b c) (list a b c))
(defun list4 (a b c d) (list a b c d))
(defun list5 (a b c d e) (list a b c d e))
(defun list6 (a b c d e f) (list a b c d e f))
(defun list7 (a b c d e f g) (list a b c d e f g))
(defun list8 (a b c d e f g h) (list a b c d e f g h))
(defun list9 (a b c d e f g h i) (list a b c d e f g h i))
(defun list10 (a b c d e f g h i j) (list a b c d e f g h i j))

;;; ---------------------------------------------------------------------
;;; cons accessors
;;; ---------------------------------------------------------------------

(defun cons.left (x) (car x))
(defun cons.right (x) (cdr x))

;;; ---------------------------------------------------------------------
;;; cons converters
;;; ---------------------------------------------------------------------

(defmethod as-cons ((x null)) x)
(defmethod as-cons ((x cons)) x)
(defmethod as-cons ((x string)) (coerce x 'list))

;;; ---------------------------------------------------------------------
;;; primitive definitions
;;; ---------------------------------------------------------------------

(defprim 'bard-symbols::|cons| 2
    (make-prim :name 'bard-symbols::|cons|
               :n-args 2
               :opcode 'cl:cons
               :always t
               :side-effects nil))

(defprim 'bard-symbols::|cons.left| 1
    (make-prim :name 'bard-symbols::|cons.left|
               :n-args 1
               :opcode 'bard::cons.left
               :always t
               :side-effects nil))

(defprim 'bard-symbols::|cons.right| 1
    (make-prim :name 'bard-symbols::|cons.right|
               :n-args 1
               :opcode 'bard::cons.right
               :always t
               :side-effects nil))

(defprim 'bard-symbols::|as-cons| 1
    (make-prim :name 'bard-symbols::|as-cons|
               :n-args 1
               :opcode 'bard::as-cons
               :always t
               :side-effects nil))


;;; ---------------------------------------------------------------------
;;; temporary list primitive
;;; ---------------------------------------------------------------------

(defprim 'bard-symbols::|list| 0
    (make-prim :name 'bard-symbols::|list|
               :n-args 0
               :opcode 'bard::list0
               :always t
               :side-effects nil))

(defprim 'bard-symbols::|list| 1
    (make-prim :name 'bard-symbols::|list|
               :n-args 1
               :opcode 'bard::list1
               :always t
               :side-effects nil))

(defprim 'bard-symbols::|list| 2
    (make-prim :name 'bard-symbols::|list|
               :n-args 2
               :opcode 'bard::list2
               :always t
               :side-effects nil))

(defprim 'bard-symbols::|list| 3
    (make-prim :name 'bard-symbols::|list|
               :n-args 3
               :opcode 'bard::list3
               :always t
               :side-effects nil))

(defprim 'bard-symbols::|list| 4
    (make-prim :name 'bard-symbols::|list|
               :n-args 4
               :opcode 'bard::list4
               :always t
               :side-effects nil))

(defprim 'bard-symbols::|list| 5
    (make-prim :name 'bard-symbols::|list|
               :n-args 5
               :opcode 'bard::list5
               :always t
               :side-effects nil))

(defprim 'bard-symbols::|list| 6
    (make-prim :name 'bard-symbols::|list|
               :n-args 6
               :opcode 'bard::list6
               :always t
               :side-effects nil))

(defprim 'bard-symbols::|list| 7
    (make-prim :name 'bard-symbols::|list|
               :n-args 7
               :opcode 'bard::list7
               :always t
               :side-effects nil))

(defprim 'bard-symbols::|list| 8
    (make-prim :name 'bard-symbols::|list|
               :n-args 8
               :opcode 'bard::list8
               :always t
               :side-effects nil))

(defprim 'bard-symbols::|list| 9
    (make-prim :name 'bard-symbols::|list|
               :n-args 9
               :opcode 'bard::list9
               :always t
               :side-effects nil))

(defprim 'bard-symbols::|list| 10
    (make-prim :name 'bard-symbols::|list|
               :n-args 10
               :opcode 'bard::list10
               :always t
               :side-effects nil))
