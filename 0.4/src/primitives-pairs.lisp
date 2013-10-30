;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          primitives-pairs.lisp
;;;; Project:       Bard
;;;; Purpose:       primitives for working with pairs and lists
;;;; Author:        mikel evins
;;;; Copyright:     2013 mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard)

(defprim 'bard-symbols::|pair| 2
    (make-prim :name 'bard-symbols::|pair|
               :n-args 2
               :opcode 'cl:cons
               :always t
               :side-effects nil))

(defprim 'bard-symbols::|pair.left| 1
    (make-prim :name 'bard-symbols::|pair.left|
               :n-args 1
               :opcode 'bard::pair.left
               :always t
               :side-effects nil))

(defprim 'bard-symbols::|pair.right| 1
    (make-prim :name 'bard-symbols::|pair.right|
               :n-args 1
               :opcode 'bard::pair.right
               :always t
               :side-effects nil))

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
