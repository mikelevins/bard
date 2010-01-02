;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: asdf -*-
;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          compiler.lisp
;;;; Project:       Bard - a near-minimal Cocoa application
;;;; Purpose:       the Bard compiler:
;;;;                ast -> Lisp code
;;;; Author:        mikel evins
;;;; Copyright:     2009 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard)

(defmethod compile ((exp bint::expression)(env bint::environment))
  (error "Unrecognized expression: ~s" exp))

;;; Void -> bard::void
(defmethod compile ((exp bint::void-expression)(env bint::environment))
  (void))

;;; Number -> CL:NUMBER
(defmethod compile ((exp bint::number-expression)(env bint::environment))
  (bint::value exp))

;;; Character -> CL::CHARACTER
(defmethod compile ((exp bint::character-expression)(env bint::environment))
  (bint::value exp))

;;; Symbol -> CL::SYMBOL
;;; TODO: compile variable lookup
(defmethod compile ((exp bint::symbol-expression)(env bint::environment))
  (error "Not yet implemented: compile (Symbol)"))

;;; Keyword -> CL::Keyword
(defmethod compile ((exp bint::keyword-expression)(env bint::environment))
  (cl:intern (bint::value exp)
             (find-package :keyword)))

;;; True -> CL::T
(defmethod compile ((exp bint::true-expression)(env bint::environment))
  t)

;;; False -> CL::NIL
(defmethod compile ((exp bint::false-expression)(env bint::environment))
  nil)

;;; Text -> CL::STRING
(defmethod compile ((exp bint::text-expression)(env bint::environment))
  (bint::value exp))

;;; Sequences

;;; Empty Sequence
(defmethod compile ((exp bint::empty-sequence-expression)(env bint::environment))
  (empty-sequence))

;;; Sequence
;;; TODO: compile special forms, macros, and funcalls
(defmethod compile ((exp bint::sequence-expression)(env bint::environment))
  (error "Not yet implemented: compile (Sequence)"))


;;; Maps

;;; Empty Sequence
(defmethod compile ((exp bint::empty-map-expression)(env bint::environment))
  (empty-map))

;;; Map
;;; TODO: compile Map expressions (must compile subexpressions in key and val positions)
(defmethod compile ((exp bint::map-expression)(env bint::environment))
  (error "Not yet implemented: compile (Map)"))
